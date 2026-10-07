#!/usr/bin/env python3
import argparse
import subprocess
import pandas as pd
from Bio import SeqIO
from Bio.Seq import Seq
from Bio.SeqRecord import SeqRecord
import os

def parse_args():
    parser = argparse.ArgumentParser(description="Automated Mirusvirus ORF stitching targeting HEG-containing introns.")
    parser.add_argument("-i", "--hmm_tsv", required=True, help="Parsed target HMMER domtblout result (TSV format)")
    parser.add_argument("-f", "--faa", required=True, help="Prodigal protein FASTA file")
    parser.add_argument("-c", "--contigs", required=True, help="Contigs nucleotide FASTA file")
    parser.add_argument("-b", "--blastdb", required=True, help="Path to local BLASTn database")
    parser.add_argument("--heg_hmm", required=True, help="Path to HMM database of Homing Endonucleases")
    parser.add_argument("-m", "--hmm", required=True, help="Name of the main HMM profile (e.g., MCP, Snf2)")
    parser.add_argument("-o", "--output", required=True, help="Output FASTA file")
    parser.add_argument("-l", "--log", required=True, help="Output log file")
    parser.add_argument("--id_map", required=True, help="Output TSV file mapping final IDs to original ORFs")
    
    # Stringency filter parameters
    parser.add_argument("--min_cov", type=float, default=30.0, help="Minimum query coverage (qcovs) percentage (default: 30.0)")
    parser.add_argument("--min_id", type=float, default=60.0, help="Minimum identity (pident) percentage (default: 60.0)")
    return parser.parse_args()

def run_blastn(seq, blastdb, min_cov, min_id):
    temp_fasta = "temp_igr.fasta"
    with open(temp_fasta, "w") as f:
        f.write(f">temp\n{seq}\n")
    
    cmd = [
        "blastn", "-query", temp_fasta, "-db", blastdb,
        "-outfmt", "6 qseqid sseqid evalue qcovs pident", 
        "-evalue", "1e-5", "-max_target_seqs", "1" 
    ]
    
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
        lines = result.stdout.strip().split('\n')
        if lines and lines[0]:
            parts = lines[0].split('\t')
            evalue = float(parts[2])
            qcovs = float(parts[3])
            pident = float(parts[4]) 
            
            os.remove(temp_fasta)
            
            # Double check: Coverage and Identity must both meet thresholds
            if qcovs >= min_cov and pident >= min_id:
                return True, evalue, qcovs, pident
            else:
                return False, None, None, None
        os.remove(temp_fasta)
        return False, None, None, None
    except subprocess.CalledProcessError:
        if os.path.exists(temp_fasta):
            os.remove(temp_fasta)
        return False, None, None, None

def run_heg_hmmsearch(seq_nt, heg_hmm_db):
    seq_obj = Seq(seq_nt)
    frames = []
    
    for i in range(3):
        s = seq_obj[i:]
        s = s[:len(s) - (len(s) % 3)]
        frames.append(SeqRecord(s.translate(), id=f"frame_f{i+1}"))
        
    rev_seq_obj = seq_obj.reverse_complement()
    for i in range(3):
        s = rev_seq_obj[i:]
        s = s[:len(s) - (len(s) % 3)]
        frames.append(SeqRecord(s.translate(), id=f"frame_r{i+1}"))
        
    temp_faa = "temp_igr_frames.faa"
    temp_out = "temp_heg_out.tsv"
    SeqIO.write(frames, temp_faa, "fasta")
    
    cmd = [
        "hmmsearch", "--noali", "-E", "1e-5", 
        "--tblout", temp_out, heg_hmm_db, temp_faa
    ]
    
    try:
        subprocess.run(cmd, capture_output=True, text=True, check=True)
        has_hit = False
        with open(temp_out, 'r') as f:
            for line in f:
                if not line.startswith('#'):
                    has_hit = True
                    break
        os.remove(temp_faa)
        os.remove(temp_out)
        return has_hit
    except subprocess.CalledProcessError:
        if os.path.exists(temp_faa): os.remove(temp_faa)
        if os.path.exists(temp_out): os.remove(temp_out)
        return False

def parse_prodigal_faa(faa_file):
    orf_info = {}
    for record in SeqIO.parse(faa_file, "fasta"):
        parts = record.description.split(" # ")
        if len(parts) >= 4:
            orf_id = record.id
            start = int(parts[1])
            end = int(parts[2])
            strand = "+" if parts[3] == "1" else "-"
            orf_info[orf_id] = {
                'seq': str(record.seq),
                'phy_start': start,
                'phy_end': end,
                'strand': strand
            }
    return orf_info

def main():
    args = parse_args()
    print("Loading sequence files...")
    orf_info = parse_prodigal_faa(args.faa)
    contigs_dict = SeqIO.to_dict(SeqIO.parse(args.contigs, "fasta"))
    hmm_df = pd.read_csv(args.hmm_tsv, sep='\t')
    hmm_df = hmm_df.sort_values(by=['contig', 'phy_start']).reset_index(drop=True)

    final_records = []  
    log_data = []
    id_mapping_data = []
    processed_orfs = set() 

    print(f"Starting stitching process (BLAST E<=1e-5, qcovs>={args.min_cov}%, pident>={args.min_id}%)...")

    for i in range(len(hmm_df) - 1):
        row1 = hmm_df.iloc[i]
        if row1['orf_id'] in processed_orfs: 
            continue
            
        row2 = hmm_df.iloc[i+1]
        
        if row1['contig'] == row2['contig'] and row1['strand'] == row2['strand']:
            phy_distance = row2['phy_start'] - row1['phy_end'] - 1
            
            if 0 < phy_distance < 3000:
                hmm_gap = row2['hmm_from'] - row1['hmm_to']
                
                if -10 <= hmm_gap <= 10:
                    contig_seq = str(contigs_dict[row1['contig']].seq)
                    igr_seq = contig_seq[int(row1['phy_end']): int(row2['phy_start'])-1]
                    
                    decision = "Condition C: Unknown large gap (Dropped)"
                    
                    if phy_distance < 50:
                        decision = "Condition B: Assembly Artifact/Frameshift"
                    else:
                        has_blast_hit, hit_evalue, hit_cov, hit_id = run_blastn(igr_seq, args.blastdb, args.min_cov, args.min_id)
                        if has_blast_hit:
                            decision = f"Condition A1: Intron Confirmed (E={hit_evalue}, Cov={hit_cov}%, Id={hit_id}%)"
                        else:
                            has_heg_hit = run_heg_hmmsearch(igr_seq, args.heg_hmm)
                            if has_heg_hit:
                                decision = "Condition A2: Intron Confirmed (HEG Domain)"

                    if "Condition A1" in decision or "Condition A2" in decision or "Condition B" in decision:
                        seq1_full = str(orf_info[row1['orf_id']]['seq'])
                        seq2_full = str(orf_info[row2['orf_id']]['seq'])
                        
                        if seq1_full.endswith('*'):
                            seq1_full = seq1_full[:-1]
                        
                        stitched_seq = seq1_full + seq2_full
                        
                        # Generate custom stitched ID
                        orf1_str = str(row1['orf_id'])
                        orf2_str = str(row2['orf_id'])
                        
                        if '_' in orf1_str and '_' in orf2_str:
                            base_name, num1 = orf1_str.rsplit('_', 1)
                            _, num2 = orf2_str.rsplit('_', 1)
                            new_id = f"{base_name}_{num1}{num2}concentrated"
                        else:
                            new_id = f"{orf1_str}_{orf2_str}concentrated"
                        
                        final_records.append(SeqRecord(Seq(stitched_seq), id=new_id, description=decision))
                        
                        processed_orfs.add(row1['orf_id'])
                        processed_orfs.add(row2['orf_id'])
                        
                        id_mapping_data.append({
                            'Final_ID': new_id,
                            'Sequence_Type': 'Stitched (With Intron/Frameshift)',
                            'Original_ORFs': f"{row1['orf_id']}, {row2['orf_id']}",
                            'Details': decision
                        })
                        
                    log_data.append({
                        'Contig': row1['contig'], 'ORF1': row1['orf_id'], 'ORF2': row2['orf_id'],
                        'Physical_Distance': phy_distance, 'HMM_Gap': hmm_gap, 
                        'Decision': decision
                    })

    single_orf_count = 0
    for i in range(len(hmm_df)):
        row = hmm_df.iloc[i]
        orf_id = row['orf_id']
        
        if orf_id not in processed_orfs:
            seq_full = str(orf_info[orf_id]['seq'])
            if seq_full.endswith('*'):
                seq_full = seq_full[:-1]
            
            new_id = str(orf_id)
            final_records.append(SeqRecord(Seq(seq_full), id=new_id, description="Single Intact Full ORF"))
            single_orf_count += 1
            
            id_mapping_data.append({
                'Final_ID': new_id,
                'Sequence_Type': 'Single (No Intron / Intact)',
                'Original_ORFs': orf_id,
                'Details': "Direct Output"
            })

    SeqIO.write(final_records, args.output, "fasta")
    pd.DataFrame(log_data).to_csv(args.log, sep='\t', index=False)
    pd.DataFrame(id_mapping_data).to_csv(args.id_map, sep='\t', index=False)
    
    print("========================================")
    print("Processing complete!")
    print(f"  - Stitched {len(final_records) - single_orf_count} fragmented sequences")
    print(f"  - Extracted {single_orf_count} complete single ORF sequences")
    print("========================================")

if __name__ == "__main__":
    main()
