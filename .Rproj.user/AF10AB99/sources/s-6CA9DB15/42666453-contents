

package.list <- c("here", #so I don't have to deal with setting a WD
                  "ape", #calculating genetic distance
                  "seqinr",
                  "rentrez"
)

#instalar pacotes novos (se ainda não estiver presente)
new.packages <- package.list[!(package.list %in% installed.packages()
                               [,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#carregar pacotes
for(i in package.list){library(i, character.only = T)}

accession_numbers <- c("KF561924.1", "MK264366.1", "MT304067.1", "JQ626278.1", 
                      "MG718915.1", "DQ660633.1", "GU135249.1", "MG833454.1", 
                      "MG833464.1", "MG718372.1", "MG718540.1", "MG718433.1", 
                      "KJ594145.1", "MG718476.1", "MT304129.1", "MT304130.1", 
                      "MG718401.1", "MT304168.1", "MG833516.1", "MG718403.1",
                      "MG833521.1", "MG833523.1", "MT304243.1", "MG833529.1", 
                      "MG833534.1", "MG833541.1", "MG718470.1", "MG833543.1", 
                      "MG833545.1", "MG833547.1", "MG833550.1", "MG833552.1", 
                      "AY904400.1", "MG718203.1", "MG718347.1", "MT304223.1", 
                      "KF561944.1", "KF561958.1", "MG833592.1", "MG718316.1", 
                      "MG833598.1", "MG833600.1", "MT304316.1", "KX640864.1", 
                      "MG718382.1", "DQ238055.1", "MG833610.1", "JQ626153.1", 
                      "MG718549.1", "MT304263.1", "JQ626046.1", "MG718328.1", 
                      "MG718306.1", "MG718474.1", "MG718417.1", "MG718447.1", 
                      "MG718304.1", "MH191262.1", "KF561935.1", "MG718334.1", 
                      "MG718481.1", "MG833635.1", "MG718504.1", "MG718457.1", 
                      "MG718343.1", "MG718511.1", "MG718354.1", "MG718582.1", 
                      "MG718358.1", "MG718508.1", "MG718434.1", "MH095607.1", 
                      "MG833688.1", "MT304369.1", "MG833695.1", "MG718538.1", 
                      "JQ626222.1", "MG833696.1", "MG718421.1", "MG718402.1", 
                      "MG833701.1", "MG718475.1", "MG833717.1", "MG718403.1", 
                      "MT304413.1", "MG718377.1")

plot_spp <- read.GenBank(accession_numbers, species.names = TRUE)

head(plot_spp)

attr(plot_spp, "species")

plot_spp_GenBank_IDs <- paste(attr(plot_spp, "species"), names
                                       (plot_spp), sep ="_RAG1_")

write.dna(plot_spp, file ="plot_spp.fasta", format = "fasta", append =
            FALSE, nbcol = 6, colsep = " ", colw = 10)

########### Some relevant arguments for write.dna()
  #x: a list or a matrix of DNA sequences.
  
  #file: a file name specified to contain our sequences
  
  #format: Three choices are possible: "interleaved", "sequential", or "fasta", or any
  #unambiguous abbreviation of these.
  
  #append: a logical, if TRUE the data are appended to the file without erasing the data
  #possibly existing in the file, otherwise the file is overwritten (FALSE the default).!
  
  #nbcol: a numeric specifying the number of columns per row (6 by default)!
  
  #colsep: a character used to separate the columns (a single space by default).!
  
  #colw: a  numeric specifying the number of nucleotides per column (10 by default).!
  
###########

seq_seqinr_format <- read.fasta(file = "plot_spp.fasta", seqtype = "DNA",
                                       as.string = TRUE, 
                                       forceDNAtolower = FALSE)

write.fasta(sequences = seq_seqinr_format,
            names = plot_spp_GenBank_IDs,
            nbchar = 10, file.out = "seq_names.fasta")

#--------

guajava_rbcL <- "Psidium guajava [Organism] AND rbcL[Gene]"

guajava_rbcL_search <- entrez_search(db="nuccore", term=guajava_rbcL, retmax=30) 
  #nucleotide database (nuccore) and retmax determines no more than 10 access numbers to
  #return

??entrez_search

  guajava_rbcL_search$ids #gives you the NCBI id

  guajava_rbcL_seqs <- entrez_fetch(db="nuccore", id=guajava_rbcL_search$ids,
                                        rettype="fasta")
  
  guajava_rbcL_seqs
  #notice \n (new line) delimiter. Other common delimiters are \r !
#(carriage return) and \t (tab).!

  write(guajava_rbcL_seqs, "guajava_rcbL.fasta", sep="\n") #gets sequence to a file

  