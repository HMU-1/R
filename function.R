iflog2 <- function(ex){
  qx <- as.numeric(quantile(ex, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm=T))
  LogC <- (qx[5] > 100) ||
    (qx[6]-qx[1] > 50 && qx[2] > 0) ||
    (qx[2] > 0 && qx[2] < 1 && qx[4] > 1 && qx[4] < 2)
  
  if (LogC) { ex[which(ex <= 0)] <- NaN
  exprSet <- log2(ex)
  print("log2 transform finished")}else{print("log2 transform not needed")}
}
signal <- function(DEG,logFC=1,adj.P.Val=0.05){
 DEG$Signal <- ifelse(abs(DEG$logFC)>logFC&DEG$adj.P.Val<adj.P.Val,ifelse(DEG$logFC>0,"up","down"),"no change")
 cat("adj.P.Val = ",adj.P.Val,",logFC = ",logFC)
 print(table(DEG$Signal))
 return(DEG)
}
convertgene <- function(x,from_org = "mmusculus",to_org = "hsapiens"){
  if (!require("BiocManager"))install.packages("BiocManager")
  if (!require(biomaRt))BiocManager::install("biomaRt")
  library(biomaRt)
  org1 = useMart('ensembl',dataset = paste(from_org,"_gene_ensembl",sep = ""))
  org2 = useMart('ensembl',dataset = paste(to_org,"_gene_ensembl",sep = ""))
  m2h.g <- getLDS(attributes = c("mgi_symbol"),filters = "mgi_symbol",
                  values = x,mart = org1,
                  attributesL = c("hgnc_symbol","chromosome_name","start_position"),
                  martL = org2,
                  uniqueRows = T)
  return(m2h.g)
  
}
