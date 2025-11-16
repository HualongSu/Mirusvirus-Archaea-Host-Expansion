import os
import pandas as pd
import glob
import logging

# Set logger
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger()

# Function to extract data from GFF3 file
def gff3_to_pddf(gff, ftype='', index_col=False):
    """Read a GFF3 file and convert it into a Pandas DataFrame."""
    gff_cols = ['seqid', 'source', 'ftype', 'start', 'end', 'score', 'strand', 'phase', 'attributes']
    if os.path.exists(gff) and os.stat(gff).st_size != 0:
        gff_df = pd.read_csv(gff, sep=r'\s+', names=gff_cols, comment='#', index_col=index_col)
        logger.info(f"Loaded GFF data: {gff}, total {len(gff_df)} rows.")
        
        if ftype:
            gff_df = gff_df[gff_df['ftype'] == ftype]
            logger.info(f"Filtered rows of type '{ftype}', total {len(gff_df)} rows.")
        return gff_df
    else:
        raise FileNotFoundError(f"GFF file {gff} does not exist or is empty.")

# Function to write CRISPR spacers into a FASTA file
def write_crispr_spacers_to_fasta(crispr_spacer_df, prefix, output_file):
    """Write CRISPR spacers into a FASTA file."""
    with open(output_file, 'w') as spacer_fa:
        for index, row in crispr_spacer_df.iterrows():
            spacer_fasta_record = f">{prefix}_{row['ID']}_____{row['seqid']}\n{row['Spacer']}\n"
            spacer_fa.write(spacer_fasta_record)
    logger.info(f"CRISPR spacers written to {output_file}")

# Main function
def extract_crispr_spacers(gff_file, output_dir):
    """Extract CRISPR spacers from CRISPRDetect GFF file and save them as a FASTA file."""
    # Get file prefix for output naming
    prefix = os.path.splitext(os.path.basename(gff_file))[0]

    # Extract CRISPR arrays (repeat_region)
    try:
        crispr_array_df = gff3_to_pddf(gff=gff_file, ftype='repeat_region', index_col=False)
        crispr_array_df[['ID', 'Repeat', 'Dbxref', 'OntologyTerm', 'ArrayQualScore']] = \
            crispr_array_df['attributes'].str.replace(r'[A-Za-z]+=', '', regex=True).str.split(pat=';', expand=True)
    except FileNotFoundError:
        logger.error(f"GFF file not found: {gff_file}")
        return

    # Extract CRISPR spacers (binding_site)
    crispr_spacer_df = gff3_to_pddf(gff=gff_file, ftype='binding_site', index_col=False)
    crispr_spacer_df[['ID', 'Name', 'Parent', 'Spacer', 'Dbxref', 'OntologyTerm', 'ArrayQualScore']] = \
        crispr_spacer_df['attributes'].str.replace(r'[A-Za-z]+=', '', regex=True).str.split(pat=';', expand=True)

    # Output file path
    output_fasta = os.path.join(output_dir, f"{prefix}_crispr_spacers.fna")
    
    # Write spacers into FASTA
    write_crispr_spacers_to_fasta(crispr_spacer_df, prefix, output_fasta)

# Example usage
gff_directory = ''
output_directory = ''
os.makedirs(output_directory, exist_ok=True)

# Get all GFF files
gff_files = glob.glob(os.path.join(gff_directory, '*.gff'))

# Extract CRISPR spacers for each GFF file
for gff_file in gff_files:
    extract_crispr_spacers(gff_file, output_directory)
