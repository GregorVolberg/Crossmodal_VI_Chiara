# Dieses Script liest eine CSV-Datendatei in GNU R ein.
# Beim Einlesen werden für alle Variablen Beschriftungen (comment) angelegt.
# Die Beschriftungen für Werte wird ebenfalls als Attribute (attr) abgelegt.

# GV: some modifications
library(tidyverse)

setwd("./soscisurv")
ds_file = "rdata_TheartofJudy_2022-08-11_16-32.csv"

ds = read.table(
  file=ds_file, encoding="UTF-8", fileEncoding="UTF-8",
  header = FALSE, sep = "\t", quote = "\"",
  dec = ".", row.names = "CASE",
  col.names = c(
    "CASE","SERIAL","REF","QUESTNNR","MODE","STARTED","SD01","SD02_01","SD10",
    "SD14","SD20","SD24","SD23","SD18_01","SD19_01","SD35_01","SD35_02","SD35_03",
    "SD35_04","SD35_05","SD35_06","SD33_01","SD33_02","SD33_03","SD33_04","SD33_05",
    "SD33_06","SD34_01","SD34_02","SD34_03","SD34_04","SD34_05","SD34_06","SD36_01",
    "SD36_02","SD36_03","SD36_04","SD36_05","SD36_06","SD37_01","SD37_02","SD37_03",
    "SD37_04","SD37_05","SD37_06","SD38_01","SD38_02","SD38_03","SD38_04","SD38_05",
    "SD38_06","SD39_01","SD39_02","SD39_03","SD39_04","SD39_05","SD39_06","SD40_01",
    "SD40_02","SD40_03","SD40_04","SD40_05","SD40_06","SD41_01","SD41_02","SD41_03",
    "SD41_04","SD41_05","SD41_06","SD42_01","SD42_02","SD42_03","SD42_04","SD42_05",
    "SD42_06","SD43_01","SD43_02","SD43_03","SD43_04","SD43_05","SD43_06","SD44_01",
    "SD44_02","SD44_03","SD44_04","SD44_05","SD44_06","SD45_01","SD45_02","SD45_03",
    "SD45_04","SD45_05","SD45_06","SD46_01","SD46_02","SD46_03","SD46_04","SD46_05",
    "SD46_06","SD47_01","SD47_02","SD47_03","SD47_04","SD47_05","SD47_06","SD48_01",
    "SD48_02","SD48_03","SD48_04","SD48_05","SD48_06","SD49_01","SD49_02","SD49_03",
    "SD49_04","SD49_05","SD49_06","SD50_01","SD50_02","SD50_03","SD50_04","SD50_05",
    "SD50_06","SD51_01","SD51_02","SD51_03","SD51_04","SD51_05","SD51_06","SD52_01",
    "SD52_02","SD52_03","SD52_04","SD52_05","SD52_06","SD53_01","SD53_02","SD53_03",
    "SD53_04","SD53_05","SD53_06","SD54_01","SD54_02","SD54_03","SD54_04","SD54_05",
    "SD54_06","SD60_01","SD60_02","SD60_03","SD60_04","SD60_05","SD60_06","SD61_01",
    "SD61_02","SD61_03","SD61_04","SD61_05","SD61_06","SD62_01","SD62_02","SD62_03",
    "SD62_04","SD62_05","SD62_06","SD63_01","SD63_02","SD63_03","SD63_04","SD63_05",
    "SD63_06","SD64_01","SD64_02","SD64_03","SD64_04","SD64_05","SD64_06","SD55_01",
    "SD55_02","SD55_03","SD55_04","SD55_05","SD55_06","SD56_01","SD56_02","SD56_03",
    "SD56_04","SD56_05","SD56_06","SD57_01","SD57_02","SD57_03","SD57_04","SD57_05",
    "SD57_06","SD29_01","SD30_01","SD32","SD65","TIME001","TIME002","TIME003",
    "TIME004","TIME005","TIME006","TIME007","TIME008","TIME009","TIME010","TIME011",
    "TIME012","TIME013","TIME014","TIME015","TIME016","TIME017","TIME018","TIME019",
    "TIME020","TIME021","TIME022","TIME023","TIME024","TIME025","TIME026","TIME027",
    "TIME028","TIME029","TIME030","TIME031","TIME032","TIME033","TIME034","TIME035",
    "TIME036","TIME037","TIME038","TIME039","TIME040","TIME041","TIME042","TIME043",
    "TIME044","TIME045","TIME046","TIME_SUM","MAILSENT","LASTDATA","FINISHED",
    "Q_VIEWER","LASTPAGE","MAXPAGE","MISSING","MISSREL","TIME_RSI","DEG_TIME"
  ),
  as.is = TRUE,
  colClasses = c(
    CASE="numeric", SERIAL="character", REF="character", QUESTNNR="character",
    MODE="factor", STARTED="POSIXct", SD01="numeric", SD02_01="numeric",
    SD10="numeric", SD14="numeric", SD20="numeric", SD24="numeric",
    SD23="numeric", SD18_01="character", SD19_01="character", SD35_01="numeric",
    SD35_02="numeric", SD35_03="numeric", SD35_04="numeric", SD35_05="numeric",
    SD35_06="numeric", SD33_01="numeric", SD33_02="numeric", SD33_03="numeric",
    SD33_04="numeric", SD33_05="numeric", SD33_06="numeric", SD34_01="numeric",
    SD34_02="numeric", SD34_03="numeric", SD34_04="numeric", SD34_05="numeric",
    SD34_06="numeric", SD36_01="numeric", SD36_02="numeric", SD36_03="numeric",
    SD36_04="numeric", SD36_05="numeric", SD36_06="numeric", SD37_01="numeric",
    SD37_02="numeric", SD37_03="numeric", SD37_04="numeric", SD37_05="numeric",
    SD37_06="numeric", SD38_01="numeric", SD38_02="numeric", SD38_03="numeric",
    SD38_04="numeric", SD38_05="numeric", SD38_06="numeric", SD39_01="numeric",
    SD39_02="numeric", SD39_03="numeric", SD39_04="numeric", SD39_05="numeric",
    SD39_06="numeric", SD40_01="numeric", SD40_02="numeric", SD40_03="numeric",
    SD40_04="numeric", SD40_05="numeric", SD40_06="numeric", SD41_01="numeric",
    SD41_02="numeric", SD41_03="numeric", SD41_04="numeric", SD41_05="numeric",
    SD41_06="numeric", SD42_01="numeric", SD42_02="numeric", SD42_03="numeric",
    SD42_04="numeric", SD42_05="numeric", SD42_06="numeric", SD43_01="numeric",
    SD43_02="numeric", SD43_03="numeric", SD43_04="numeric", SD43_05="numeric",
    SD43_06="numeric", SD44_01="numeric", SD44_02="numeric", SD44_03="numeric",
    SD44_04="numeric", SD44_05="numeric", SD44_06="numeric", SD45_01="numeric",
    SD45_02="numeric", SD45_03="numeric", SD45_04="numeric", SD45_05="numeric",
    SD45_06="numeric", SD46_01="numeric", SD46_02="numeric", SD46_03="numeric",
    SD46_04="numeric", SD46_05="numeric", SD46_06="numeric", SD47_01="numeric",
    SD47_02="numeric", SD47_03="numeric", SD47_04="numeric", SD47_05="numeric",
    SD47_06="numeric", SD48_01="numeric", SD48_02="numeric", SD48_03="numeric",
    SD48_04="numeric", SD48_05="numeric", SD48_06="numeric", SD49_01="numeric",
    SD49_02="numeric", SD49_03="numeric", SD49_04="numeric", SD49_05="numeric",
    SD49_06="numeric", SD50_01="numeric", SD50_02="numeric", SD50_03="numeric",
    SD50_04="numeric", SD50_05="numeric", SD50_06="numeric", SD51_01="numeric",
    SD51_02="numeric", SD51_03="numeric", SD51_04="numeric", SD51_05="numeric",
    SD51_06="numeric", SD52_01="numeric", SD52_02="numeric", SD52_03="numeric",
    SD52_04="numeric", SD52_05="numeric", SD52_06="numeric", SD53_01="numeric",
    SD53_02="numeric", SD53_03="numeric", SD53_04="numeric", SD53_05="numeric",
    SD53_06="numeric", SD54_01="numeric", SD54_02="numeric", SD54_03="numeric",
    SD54_04="numeric", SD54_05="numeric", SD54_06="numeric", SD60_01="numeric",
    SD60_02="numeric", SD60_03="numeric", SD60_04="numeric", SD60_05="numeric",
    SD60_06="numeric", SD61_01="numeric", SD61_02="numeric", SD61_03="numeric",
    SD61_04="numeric", SD61_05="numeric", SD61_06="numeric", SD62_01="numeric",
    SD62_02="numeric", SD62_03="numeric", SD62_04="numeric", SD62_05="numeric",
    SD62_06="numeric", SD63_01="numeric", SD63_02="numeric", SD63_03="numeric",
    SD63_04="numeric", SD63_05="numeric", SD63_06="numeric", SD64_01="numeric",
    SD64_02="numeric", SD64_03="numeric", SD64_04="numeric", SD64_05="numeric",
    SD64_06="numeric", SD55_01="numeric", SD55_02="numeric", SD55_03="numeric",
    SD55_04="numeric", SD55_05="numeric", SD55_06="numeric", SD56_01="numeric",
    SD56_02="numeric", SD56_03="numeric", SD56_04="numeric", SD56_05="numeric",
    SD56_06="numeric", SD57_01="numeric", SD57_02="numeric", SD57_03="numeric",
    SD57_04="numeric", SD57_05="numeric", SD57_06="numeric",
    SD29_01="character", SD30_01="character", SD32="numeric", SD65="numeric",
    TIME001="integer", TIME002="integer", TIME003="integer", TIME004="integer",
    TIME005="integer", TIME006="integer", TIME007="integer", TIME008="integer",
    TIME009="integer", TIME010="integer", TIME011="integer", TIME012="integer",
    TIME013="integer", TIME014="integer", TIME015="integer", TIME016="integer",
    TIME017="integer", TIME018="integer", TIME019="integer", TIME020="integer",
    TIME021="integer", TIME022="integer", TIME023="integer", TIME024="integer",
    TIME025="integer", TIME026="integer", TIME027="integer", TIME028="integer",
    TIME029="integer", TIME030="integer", TIME031="integer", TIME032="integer",
    TIME033="integer", TIME034="integer", TIME035="integer", TIME036="integer",
    TIME037="integer", TIME038="integer", TIME039="integer", TIME040="integer",
    TIME041="integer", TIME042="integer", TIME043="integer", TIME044="integer",
    TIME045="integer", TIME046="integer", TIME_SUM="integer",
    MAILSENT="POSIXct", LASTDATA="POSIXct", FINISHED="logical",
    Q_VIEWER="logical", LASTPAGE="numeric", MAXPAGE="numeric",
    MISSING="numeric", MISSREL="numeric", TIME_RSI="numeric", DEG_TIME="numeric"
  ),
  skip = 1,
  check.names = TRUE, fill = TRUE,
  strip.white = FALSE, blank.lines.skip = TRUE,
  comment.char = "",
  na.strings = ""
)

rm(ds_file)

attr(ds, "project") = "TheartofJudy"
attr(ds, "description") = "Kunst und Töne"
attr(ds, "date") = "2022-08-11 16:32:15"
attr(ds, "server") = "https://www.soscisurvey.de"

# Variable und Value Labels
ds$SD01 = factor(ds$SD01, levels=c("1","2","3","-9"), labels=c("weiblich","männlich","divers","[NA] nicht beantwortet"), ordered=FALSE)
ds$SD10 = factor(ds$SD10, levels=c("1","2","3","4","5","6","7","8","9","-9"), labels=c("Ich bin noch Schüler*in","Schule beendet ohne Abschluss","Hauptschulabschluss/Volksschulabschluss","Realschulabschluss (Mittlere Reife)","<5>","Fachhochschulreife (Abschluss einer Fachoberschule)","Abitur, allgemeine oder fachgebundene Hochschulreife (Gymnasium bzw. EOS)","Hochschulabschluss","Anderer Schulabschluss:","[NA] nicht beantwortet"), ordered=FALSE)
ds$SD14 = factor(ds$SD14, levels=c("1","2","3","4","5","6","7","8","-9"), labels=c("Schüler*in","Auszubildende*r","Student*in","Angestellte*r","Beamt*in","Selbstständig","Arbeitslos/Arbeit suchend","Sonstiges:","[NA] nicht beantwortet"), ordered=FALSE)
ds$SD20 = factor(ds$SD20, levels=c("1","2","3","4","-9"), labels=c("Nein, ich habe keine Hörbeeinträchtigung","Ja, ich habe eine Hörbeeinträchtigung und trage ein Hörgerät","Ja, ich habe eine Hörbeeinträchtigung, aber trage kein Hörgerät","Ich weiß es nicht","[NA] nicht beantwortet"), ordered=FALSE)
ds$SD24 = factor(ds$SD24, levels=c("1","2","3","4","-9"), labels=c("Nein ich habe keine Sehschwäche","Ja, ich habe eine Sehschwäche und habe eine Sehhilfe (z.b. Brille oder Kontaktlinsen)","Ja, ich habe eine Sehschwäche, aber trage keine Sehhilfe (z.b. Brille oder Kontaktlinsen)","Ich weiß es nicht","[NA] nicht beantwortet"), ordered=FALSE)
ds$SD23 = factor(ds$SD23, levels=c("1","2","3","-9"), labels=c("Ja, ich habe mich voll auf das Experiment konzentrieren können","Die Konzentration aufrecht zu halten, fiel mir teilweise schwer","Nein, ich habe mich überhaupt nicht auf das Experiment konzentrieren können.","[NA] nicht beantwortet"), ordered=FALSE)
ds$SD32 = factor(ds$SD32, levels=c("1","-9"), labels=c("Ja, ich habe diese Information in Kenntnis genommen","[NA] nicht beantwortet"), ordered=FALSE)
ds$SD65 = factor(ds$SD65, levels=c("1","-9"), labels=c("Ja, ich habe die dies verstanden und bin damit einverstanden","[NA] nicht beantwortet"), ordered=FALSE)
attr(ds$SD35_01,"1") = "überhaupt nicht"
attr(ds$SD35_01,"5") = "äußerst"
attr(ds$SD35_02,"1") = "überhaupt nicht"
attr(ds$SD35_02,"5") = "äußerst"
attr(ds$SD35_03,"1") = "überhaupt nicht"
attr(ds$SD35_03,"5") = "äußerst"
attr(ds$SD35_04,"1") = "überhaupt nicht"
attr(ds$SD35_04,"5") = "äußerst"
attr(ds$SD35_05,"1") = "überhaupt nicht"
attr(ds$SD35_05,"5") = "äußerst"
attr(ds$SD35_06,"1") = "überhaupt nicht"
attr(ds$SD35_06,"5") = "äußerst"
attr(ds$SD33_01,"1") = "überhaupt nicht"
attr(ds$SD33_01,"5") = "äußerst"
attr(ds$SD33_02,"1") = "überhaupt nicht"
attr(ds$SD33_02,"5") = "äußerst"
attr(ds$SD33_03,"1") = "überhaupt nicht"
attr(ds$SD33_03,"5") = "äußerst"
attr(ds$SD33_04,"1") = "überhaupt nicht"
attr(ds$SD33_04,"5") = "äußerst"
attr(ds$SD33_05,"1") = "überhaupt nicht"
attr(ds$SD33_05,"5") = "äußerst"
attr(ds$SD33_06,"1") = "überhaupt nicht"
attr(ds$SD33_06,"5") = "äußerst"
attr(ds$SD34_01,"1") = "überhaupt nicht"
attr(ds$SD34_01,"5") = "äußerst"
attr(ds$SD34_02,"1") = "überhaupt nicht"
attr(ds$SD34_02,"5") = "äußerst"
attr(ds$SD34_03,"1") = "überhaupt nicht"
attr(ds$SD34_03,"5") = "äußerst"
attr(ds$SD34_04,"1") = "überhaupt nicht"
attr(ds$SD34_04,"5") = "äußerst"
attr(ds$SD34_05,"1") = "überhaupt nicht"
attr(ds$SD34_05,"5") = "äußerst"
attr(ds$SD34_06,"1") = "überhaupt nicht"
attr(ds$SD34_06,"5") = "äußerst"
attr(ds$SD36_01,"1") = "überhaupt nicht"
attr(ds$SD36_01,"5") = "äußerst"
attr(ds$SD36_02,"1") = "überhaupt nicht"
attr(ds$SD36_02,"5") = "äußerst"
attr(ds$SD36_03,"1") = "überhaupt nicht"
attr(ds$SD36_03,"5") = "äußerst"
attr(ds$SD36_04,"1") = "überhaupt nicht"
attr(ds$SD36_04,"5") = "äußerst"
attr(ds$SD36_05,"1") = "überhaupt nicht"
attr(ds$SD36_05,"5") = "äußerst"
attr(ds$SD36_06,"1") = "überhaupt nicht"
attr(ds$SD36_06,"5") = "äußerst"
attr(ds$SD37_01,"1") = "überhaupt nicht"
attr(ds$SD37_01,"5") = "äußerst"
attr(ds$SD37_02,"1") = "überhaupt nicht"
attr(ds$SD37_02,"5") = "äußerst"
attr(ds$SD37_03,"1") = "überhaupt nicht"
attr(ds$SD37_03,"5") = "äußerst"
attr(ds$SD37_04,"1") = "überhaupt nicht"
attr(ds$SD37_04,"5") = "äußerst"
attr(ds$SD37_05,"1") = "überhaupt nicht"
attr(ds$SD37_05,"5") = "äußerst"
attr(ds$SD37_06,"1") = "überhaupt nicht"
attr(ds$SD37_06,"5") = "äußerst"
attr(ds$SD38_01,"1") = "überhaupt nicht"
attr(ds$SD38_01,"5") = "äußerst"
attr(ds$SD38_02,"1") = "überhaupt nicht"
attr(ds$SD38_02,"5") = "äußerst"
attr(ds$SD38_03,"1") = "überhaupt nicht"
attr(ds$SD38_03,"5") = "äußerst"
attr(ds$SD38_04,"1") = "überhaupt nicht"
attr(ds$SD38_04,"5") = "äußerst"
attr(ds$SD38_05,"1") = "überhaupt nicht"
attr(ds$SD38_05,"5") = "äußerst"
attr(ds$SD38_06,"1") = "überhaupt nicht"
attr(ds$SD38_06,"5") = "äußerst"
attr(ds$SD39_01,"1") = "überhaupt nicht"
attr(ds$SD39_01,"5") = "äußerst"
attr(ds$SD39_02,"1") = "überhaupt nicht"
attr(ds$SD39_02,"5") = "äußerst"
attr(ds$SD39_03,"1") = "überhaupt nicht"
attr(ds$SD39_03,"5") = "äußerst"
attr(ds$SD39_04,"1") = "überhaupt nicht"
attr(ds$SD39_04,"5") = "äußerst"
attr(ds$SD39_05,"1") = "überhaupt nicht"
attr(ds$SD39_05,"5") = "äußerst"
attr(ds$SD39_06,"1") = "überhaupt nicht"
attr(ds$SD39_06,"5") = "äußerst"
attr(ds$SD40_01,"1") = "überhaupt nicht"
attr(ds$SD40_01,"5") = "äußerst"
attr(ds$SD40_02,"1") = "überhaupt nicht"
attr(ds$SD40_02,"5") = "äußerst"
attr(ds$SD40_03,"1") = "überhaupt nicht"
attr(ds$SD40_03,"5") = "äußerst"
attr(ds$SD40_04,"1") = "überhaupt nicht"
attr(ds$SD40_04,"5") = "äußerst"
attr(ds$SD40_05,"1") = "überhaupt nicht"
attr(ds$SD40_05,"5") = "äußerst"
attr(ds$SD40_06,"1") = "überhaupt nicht"
attr(ds$SD40_06,"5") = "äußerst"
attr(ds$SD41_01,"1") = "überhaupt nicht"
attr(ds$SD41_01,"5") = "äußerst"
attr(ds$SD41_02,"1") = "überhaupt nicht"
attr(ds$SD41_02,"5") = "äußerst"
attr(ds$SD41_03,"1") = "überhaupt nicht"
attr(ds$SD41_03,"5") = "äußerst"
attr(ds$SD41_04,"1") = "überhaupt nicht"
attr(ds$SD41_04,"5") = "äußerst"
attr(ds$SD41_05,"1") = "überhaupt nicht"
attr(ds$SD41_05,"5") = "äußerst"
attr(ds$SD41_06,"1") = "überhaupt nicht"
attr(ds$SD41_06,"5") = "äußerst"
attr(ds$SD42_01,"1") = "überhaupt nicht"
attr(ds$SD42_01,"5") = "äußerst"
attr(ds$SD42_02,"1") = "überhaupt nicht"
attr(ds$SD42_02,"5") = "äußerst"
attr(ds$SD42_03,"1") = "überhaupt nicht"
attr(ds$SD42_03,"5") = "äußerst"
attr(ds$SD42_04,"1") = "überhaupt nicht"
attr(ds$SD42_04,"5") = "äußerst"
attr(ds$SD42_05,"1") = "überhaupt nicht"
attr(ds$SD42_05,"5") = "äußerst"
attr(ds$SD42_06,"1") = "überhaupt nicht"
attr(ds$SD42_06,"5") = "äußerst"
attr(ds$SD43_01,"1") = "überhaupt nicht"
attr(ds$SD43_01,"5") = "äußerst"
attr(ds$SD43_02,"1") = "überhaupt nicht"
attr(ds$SD43_02,"5") = "äußerst"
attr(ds$SD43_03,"1") = "überhaupt nicht"
attr(ds$SD43_03,"5") = "äußerst"
attr(ds$SD43_04,"1") = "überhaupt nicht"
attr(ds$SD43_04,"5") = "äußerst"
attr(ds$SD43_05,"1") = "überhaupt nicht"
attr(ds$SD43_05,"5") = "äußerst"
attr(ds$SD43_06,"1") = "überhaupt nicht"
attr(ds$SD43_06,"5") = "äußerst"
attr(ds$SD44_01,"1") = "überhaupt nicht"
attr(ds$SD44_01,"5") = "äußerst"
attr(ds$SD44_02,"1") = "überhaupt nicht"
attr(ds$SD44_02,"5") = "äußerst"
attr(ds$SD44_03,"1") = "überhaupt nicht"
attr(ds$SD44_03,"5") = "äußerst"
attr(ds$SD44_04,"1") = "überhaupt nicht"
attr(ds$SD44_04,"5") = "äußerst"
attr(ds$SD44_05,"1") = "überhaupt nicht"
attr(ds$SD44_05,"5") = "äußerst"
attr(ds$SD44_06,"1") = "überhaupt nicht"
attr(ds$SD44_06,"5") = "äußerst"
attr(ds$SD45_01,"1") = "überhaupt nicht"
attr(ds$SD45_01,"5") = "äußerst"
attr(ds$SD45_02,"1") = "überhaupt nicht"
attr(ds$SD45_02,"5") = "äußerst"
attr(ds$SD45_03,"1") = "überhaupt nicht"
attr(ds$SD45_03,"5") = "äußerst"
attr(ds$SD45_04,"1") = "überhaupt nicht"
attr(ds$SD45_04,"5") = "äußerst"
attr(ds$SD45_05,"1") = "überhaupt nicht"
attr(ds$SD45_05,"5") = "äußerst"
attr(ds$SD45_06,"1") = "überhaupt nicht"
attr(ds$SD45_06,"5") = "äußerst"
attr(ds$SD46_01,"1") = "überhaupt nicht"
attr(ds$SD46_01,"5") = "äußerst"
attr(ds$SD46_02,"1") = "überhaupt nicht"
attr(ds$SD46_02,"5") = "äußerst"
attr(ds$SD46_03,"1") = "überhaupt nicht"
attr(ds$SD46_03,"5") = "äußerst"
attr(ds$SD46_04,"1") = "überhaupt nicht"
attr(ds$SD46_04,"5") = "äußerst"
attr(ds$SD46_05,"1") = "überhaupt nicht"
attr(ds$SD46_05,"5") = "äußerst"
attr(ds$SD46_06,"1") = "überhaupt nicht"
attr(ds$SD46_06,"5") = "äußerst"
attr(ds$SD47_01,"1") = "überhaupt nicht"
attr(ds$SD47_01,"5") = "äußerst"
attr(ds$SD47_02,"1") = "überhaupt nicht"
attr(ds$SD47_02,"5") = "äußerst"
attr(ds$SD47_03,"1") = "überhaupt nicht"
attr(ds$SD47_03,"5") = "äußerst"
attr(ds$SD47_04,"1") = "überhaupt nicht"
attr(ds$SD47_04,"5") = "äußerst"
attr(ds$SD47_05,"1") = "überhaupt nicht"
attr(ds$SD47_05,"5") = "äußerst"
attr(ds$SD47_06,"1") = "überhaupt nicht"
attr(ds$SD47_06,"5") = "äußerst"
attr(ds$SD48_01,"1") = "überhaupt nicht"
attr(ds$SD48_01,"5") = "äußerst"
attr(ds$SD48_02,"1") = "überhaupt nicht"
attr(ds$SD48_02,"5") = "äußerst"
attr(ds$SD48_03,"1") = "überhaupt nicht"
attr(ds$SD48_03,"5") = "äußerst"
attr(ds$SD48_04,"1") = "überhaupt nicht"
attr(ds$SD48_04,"5") = "äußerst"
attr(ds$SD48_05,"1") = "überhaupt nicht"
attr(ds$SD48_05,"5") = "äußerst"
attr(ds$SD48_06,"1") = "überhaupt nicht"
attr(ds$SD48_06,"5") = "äußerst"
attr(ds$SD49_01,"1") = "überhaupt nicht"
attr(ds$SD49_01,"5") = "äußerst"
attr(ds$SD49_02,"1") = "überhaupt nicht"
attr(ds$SD49_02,"5") = "äußerst"
attr(ds$SD49_03,"1") = "überhaupt nicht"
attr(ds$SD49_03,"5") = "äußerst"
attr(ds$SD49_04,"1") = "überhaupt nicht"
attr(ds$SD49_04,"5") = "äußerst"
attr(ds$SD49_05,"1") = "überhaupt nicht"
attr(ds$SD49_05,"5") = "äußerst"
attr(ds$SD49_06,"1") = "überhaupt nicht"
attr(ds$SD49_06,"5") = "äußerst"
attr(ds$SD50_01,"1") = "überhaupt nicht"
attr(ds$SD50_01,"5") = "äußerst"
attr(ds$SD50_02,"1") = "überhaupt nicht"
attr(ds$SD50_02,"5") = "äußerst"
attr(ds$SD50_03,"1") = "überhaupt nicht"
attr(ds$SD50_03,"5") = "äußerst"
attr(ds$SD50_04,"1") = "überhaupt nicht"
attr(ds$SD50_04,"5") = "äußerst"
attr(ds$SD50_05,"1") = "überhaupt nicht"
attr(ds$SD50_05,"5") = "äußerst"
attr(ds$SD50_06,"1") = "überhaupt nicht"
attr(ds$SD50_06,"5") = "äußerst"
attr(ds$SD51_01,"1") = "überhaupt nicht"
attr(ds$SD51_01,"5") = "äußerst"
attr(ds$SD51_02,"1") = "überhaupt nicht"
attr(ds$SD51_02,"5") = "äußerst"
attr(ds$SD51_03,"1") = "überhaupt nicht"
attr(ds$SD51_03,"5") = "äußerst"
attr(ds$SD51_04,"1") = "überhaupt nicht"
attr(ds$SD51_04,"5") = "äußerst"
attr(ds$SD51_05,"1") = "überhaupt nicht"
attr(ds$SD51_05,"5") = "äußerst"
attr(ds$SD51_06,"1") = "überhaupt nicht"
attr(ds$SD51_06,"5") = "äußerst"
attr(ds$SD52_01,"1") = "überhaupt nicht"
attr(ds$SD52_01,"5") = "äußerst"
attr(ds$SD52_02,"1") = "überhaupt nicht"
attr(ds$SD52_02,"5") = "äußerst"
attr(ds$SD52_03,"1") = "überhaupt nicht"
attr(ds$SD52_03,"5") = "äußerst"
attr(ds$SD52_04,"1") = "überhaupt nicht"
attr(ds$SD52_04,"5") = "äußerst"
attr(ds$SD52_05,"1") = "überhaupt nicht"
attr(ds$SD52_05,"5") = "äußerst"
attr(ds$SD52_06,"1") = "überhaupt nicht"
attr(ds$SD52_06,"5") = "äußerst"
attr(ds$SD53_01,"1") = "überhaupt nicht"
attr(ds$SD53_01,"5") = "äußerst"
attr(ds$SD53_02,"1") = "überhaupt nicht"
attr(ds$SD53_02,"5") = "äußerst"
attr(ds$SD53_03,"1") = "überhaupt nicht"
attr(ds$SD53_03,"5") = "äußerst"
attr(ds$SD53_04,"1") = "überhaupt nicht"
attr(ds$SD53_04,"5") = "äußerst"
attr(ds$SD53_05,"1") = "überhaupt nicht"
attr(ds$SD53_05,"5") = "äußerst"
attr(ds$SD53_06,"1") = "überhaupt nicht"
attr(ds$SD53_06,"5") = "äußerst"
attr(ds$SD54_01,"1") = "überhaupt nicht"
attr(ds$SD54_01,"5") = "äußerst"
attr(ds$SD54_02,"1") = "überhaupt nicht"
attr(ds$SD54_02,"5") = "äußerst"
attr(ds$SD54_03,"1") = "überhaupt nicht"
attr(ds$SD54_03,"5") = "äußerst"
attr(ds$SD54_04,"1") = "überhaupt nicht"
attr(ds$SD54_04,"5") = "äußerst"
attr(ds$SD54_05,"1") = "überhaupt nicht"
attr(ds$SD54_05,"5") = "äußerst"
attr(ds$SD54_06,"1") = "überhaupt nicht"
attr(ds$SD54_06,"5") = "äußerst"
attr(ds$SD60_01,"1") = "überhaupt nicht"
attr(ds$SD60_01,"5") = "äußerst"
attr(ds$SD60_02,"1") = "überhaupt nicht"
attr(ds$SD60_02,"5") = "äußerst"
attr(ds$SD60_03,"1") = "überhaupt nicht"
attr(ds$SD60_03,"5") = "äußerst"
attr(ds$SD60_04,"1") = "überhaupt nicht"
attr(ds$SD60_04,"5") = "äußerst"
attr(ds$SD60_05,"1") = "überhaupt nicht"
attr(ds$SD60_05,"5") = "äußerst"
attr(ds$SD60_06,"1") = "überhaupt nicht"
attr(ds$SD60_06,"5") = "äußerst"
attr(ds$SD61_01,"1") = "überhaupt nicht"
attr(ds$SD61_01,"5") = "äußerst"
attr(ds$SD61_02,"1") = "überhaupt nicht"
attr(ds$SD61_02,"5") = "äußerst"
attr(ds$SD61_03,"1") = "überhaupt nicht"
attr(ds$SD61_03,"5") = "äußerst"
attr(ds$SD61_04,"1") = "überhaupt nicht"
attr(ds$SD61_04,"5") = "äußerst"
attr(ds$SD61_05,"1") = "überhaupt nicht"
attr(ds$SD61_05,"5") = "äußerst"
attr(ds$SD61_06,"1") = "überhaupt nicht"
attr(ds$SD61_06,"5") = "äußerst"
attr(ds$SD62_01,"1") = "überhaupt nicht"
attr(ds$SD62_01,"5") = "äußerst"
attr(ds$SD62_02,"1") = "überhaupt nicht"
attr(ds$SD62_02,"5") = "äußerst"
attr(ds$SD62_03,"1") = "überhaupt nicht"
attr(ds$SD62_03,"5") = "äußerst"
attr(ds$SD62_04,"1") = "überhaupt nicht"
attr(ds$SD62_04,"5") = "äußerst"
attr(ds$SD62_05,"1") = "überhaupt nicht"
attr(ds$SD62_05,"5") = "äußerst"
attr(ds$SD62_06,"1") = "überhaupt nicht"
attr(ds$SD62_06,"5") = "äußerst"
attr(ds$SD63_01,"1") = "überhaupt nicht"
attr(ds$SD63_01,"5") = "äußerst"
attr(ds$SD63_02,"1") = "überhaupt nicht"
attr(ds$SD63_02,"5") = "äußerst"
attr(ds$SD63_03,"1") = "überhaupt nicht"
attr(ds$SD63_03,"5") = "äußerst"
attr(ds$SD63_04,"1") = "überhaupt nicht"
attr(ds$SD63_04,"5") = "äußerst"
attr(ds$SD63_05,"1") = "überhaupt nicht"
attr(ds$SD63_05,"5") = "äußerst"
attr(ds$SD63_06,"1") = "überhaupt nicht"
attr(ds$SD63_06,"5") = "äußerst"
attr(ds$SD64_01,"1") = "überhaupt nicht"
attr(ds$SD64_01,"5") = "äußerst"
attr(ds$SD64_02,"1") = "überhaupt nicht"
attr(ds$SD64_02,"5") = "äußerst"
attr(ds$SD64_03,"1") = "überhaupt nicht"
attr(ds$SD64_03,"5") = "äußerst"
attr(ds$SD64_04,"1") = "überhaupt nicht"
attr(ds$SD64_04,"5") = "äußerst"
attr(ds$SD64_05,"1") = "überhaupt nicht"
attr(ds$SD64_05,"5") = "äußerst"
attr(ds$SD64_06,"1") = "überhaupt nicht"
attr(ds$SD64_06,"5") = "äußerst"
attr(ds$SD55_01,"1") = "überhaupt nicht"
attr(ds$SD55_01,"5") = "äußerst"
attr(ds$SD55_02,"1") = "überhaupt nicht"
attr(ds$SD55_02,"5") = "äußerst"
attr(ds$SD55_03,"1") = "überhaupt nicht"
attr(ds$SD55_03,"5") = "äußerst"
attr(ds$SD55_04,"1") = "überhaupt nicht"
attr(ds$SD55_04,"5") = "äußerst"
attr(ds$SD55_05,"1") = "überhaupt nicht"
attr(ds$SD55_05,"5") = "äußerst"
attr(ds$SD55_06,"1") = "überhaupt nicht"
attr(ds$SD55_06,"5") = "äußerst"
attr(ds$SD56_01,"1") = "überhaupt nicht"
attr(ds$SD56_01,"5") = "äußerst"
attr(ds$SD56_02,"1") = "überhaupt nicht"
attr(ds$SD56_02,"5") = "äußerst"
attr(ds$SD56_03,"1") = "überhaupt nicht"
attr(ds$SD56_03,"5") = "äußerst"
attr(ds$SD56_04,"1") = "überhaupt nicht"
attr(ds$SD56_04,"5") = "äußerst"
attr(ds$SD56_05,"1") = "überhaupt nicht"
attr(ds$SD56_05,"5") = "äußerst"
attr(ds$SD56_06,"1") = "überhaupt nicht"
attr(ds$SD56_06,"5") = "äußerst"
attr(ds$SD57_01,"1") = "überhaupt nicht"
attr(ds$SD57_01,"5") = "äußerst"
attr(ds$SD57_02,"1") = "überhaupt nicht"
attr(ds$SD57_02,"5") = "äußerst"
attr(ds$SD57_03,"1") = "überhaupt nicht"
attr(ds$SD57_03,"5") = "äußerst"
attr(ds$SD57_04,"1") = "überhaupt nicht"
attr(ds$SD57_04,"5") = "äußerst"
attr(ds$SD57_05,"1") = "überhaupt nicht"
attr(ds$SD57_05,"5") = "äußerst"
attr(ds$SD57_06,"1") = "überhaupt nicht"
attr(ds$SD57_06,"5") = "äußerst"
attr(ds$FINISHED,"F") = "abgebrochen"
attr(ds$FINISHED,"T") = "ausgefüllt"
attr(ds$Q_VIEWER,"F") = "Teilnehmer"
attr(ds$Q_VIEWER,"T") = "Durchklicker"
comment(ds$SERIAL) = "Seriennummer (sofern verwendet)"
comment(ds$REF) = "Referenz (sofern im Link angegeben)"
comment(ds$QUESTNNR) = "Fragebogen, der im Interview verwendet wurde"
comment(ds$MODE) = "Interview-Modus"
comment(ds$STARTED) = "Zeitpunkt zu dem das Interview begonnen hat (Europe/Berlin)"
comment(ds$SD01) = "Geschlecht"
comment(ds$SD02_01) = "Alter (direkt): Ich bin   ... Jahre alt"
comment(ds$SD10) = "Formale Bildung"
comment(ds$SD14) = "Beschäftigung"
comment(ds$SD20) = "Hörbeeinträchtigung"
comment(ds$SD24) = "Sehschwäche"
comment(ds$SD23) = "Konzentration"
comment(ds$SD18_01) = "Anmerkungen (offen): [01]"
comment(ds$SD19_01) = "Ziele der Studie (offen): [01]"
comment(ds$SD35_01) = "Emotion: Freude"
comment(ds$SD35_02) = "Emotion: Traurigkeit"
comment(ds$SD35_03) = "Emotion: Zorn"
comment(ds$SD35_04) = "Emotion: Angst"
comment(ds$SD35_05) = "Emotion: Ekel"
comment(ds$SD35_06) = "Emotion: Überraschung"
comment(ds$SD33_01) = "Emotion: Freude"
comment(ds$SD33_02) = "Emotion: Traurigkeit"
comment(ds$SD33_03) = "Emotion: Zorn"
comment(ds$SD33_04) = "Emotion: Angst"
comment(ds$SD33_05) = "Emotion: Ekel"
comment(ds$SD33_06) = "Emotion: Überraschung"
comment(ds$SD34_01) = "Emotion: Freude"
comment(ds$SD34_02) = "Emotion: Traurigkeit"
comment(ds$SD34_03) = "Emotion: Zorn"
comment(ds$SD34_04) = "Emotion: Angst"
comment(ds$SD34_05) = "Emotion: Ekel"
comment(ds$SD34_06) = "Emotion: Überraschung"
comment(ds$SD36_01) = "Emotion: Freude"
comment(ds$SD36_02) = "Emotion: Traurigkeit"
comment(ds$SD36_03) = "Emotion: Zorn"
comment(ds$SD36_04) = "Emotion: Angst"
comment(ds$SD36_05) = "Emotion: Ekel"
comment(ds$SD36_06) = "Emotion: Überraschung"
comment(ds$SD37_01) = "Emotion: Freude"
comment(ds$SD37_02) = "Emotion: Traurigkeit"
comment(ds$SD37_03) = "Emotion: Zorn"
comment(ds$SD37_04) = "Emotion: Angst"
comment(ds$SD37_05) = "Emotion: Ekel"
comment(ds$SD37_06) = "Emotion: Überraschung"
comment(ds$SD38_01) = "Emotion: Freude"
comment(ds$SD38_02) = "Emotion: Traurigkeit"
comment(ds$SD38_03) = "Emotion: Zorn"
comment(ds$SD38_04) = "Emotion: Angst"
comment(ds$SD38_05) = "Emotion: Ekel"
comment(ds$SD38_06) = "Emotion: Überraschung"
comment(ds$SD39_01) = "Emotion: Freude"
comment(ds$SD39_02) = "Emotion: Traurigkeit"
comment(ds$SD39_03) = "Emotion: Zorn"
comment(ds$SD39_04) = "Emotion: Angst"
comment(ds$SD39_05) = "Emotion: Ekel"
comment(ds$SD39_06) = "Emotion: Überraschung"
comment(ds$SD40_01) = "Emotion: Freude"
comment(ds$SD40_02) = "Emotion: Traurigkeit"
comment(ds$SD40_03) = "Emotion: Zorn"
comment(ds$SD40_04) = "Emotion: Angst"
comment(ds$SD40_05) = "Emotion: Ekel"
comment(ds$SD40_06) = "Emotion: Überraschung"
comment(ds$SD41_01) = "Emotion: Freude"
comment(ds$SD41_02) = "Emotion: Traurigkeit"
comment(ds$SD41_03) = "Emotion: Zorn"
comment(ds$SD41_04) = "Emotion: Angst"
comment(ds$SD41_05) = "Emotion: Ekel"
comment(ds$SD41_06) = "Emotion: Überraschung"
comment(ds$SD42_01) = "Emotion: Freude"
comment(ds$SD42_02) = "Emotion: Traurigkeit"
comment(ds$SD42_03) = "Emotion: Zorn"
comment(ds$SD42_04) = "Emotion: Angst"
comment(ds$SD42_05) = "Emotion: Ekel"
comment(ds$SD42_06) = "Emotion: Überraschung"
comment(ds$SD43_01) = "Emotion: Freude"
comment(ds$SD43_02) = "Emotion: Traurigkeit"
comment(ds$SD43_03) = "Emotion: Zorn"
comment(ds$SD43_04) = "Emotion: Angst"
comment(ds$SD43_05) = "Emotion: Ekel"
comment(ds$SD43_06) = "Emotion: Überraschung"
comment(ds$SD44_01) = "Emotion: Freude"
comment(ds$SD44_02) = "Emotion: Traurigkeit"
comment(ds$SD44_03) = "Emotion: Zorn"
comment(ds$SD44_04) = "Emotion: Angst"
comment(ds$SD44_05) = "Emotion: Ekel"
comment(ds$SD44_06) = "Emotion: Überraschung"
comment(ds$SD45_01) = "Emotion: Freude"
comment(ds$SD45_02) = "Emotion: Traurigkeit"
comment(ds$SD45_03) = "Emotion: Zorn"
comment(ds$SD45_04) = "Emotion: Angst"
comment(ds$SD45_05) = "Emotion: Ekel"
comment(ds$SD45_06) = "Emotion: Überraschung"
comment(ds$SD46_01) = "Emotion: Freude"
comment(ds$SD46_02) = "Emotion: Traurigkeit"
comment(ds$SD46_03) = "Emotion: Zorn"
comment(ds$SD46_04) = "Emotion: Angst"
comment(ds$SD46_05) = "Emotion: Ekel"
comment(ds$SD46_06) = "Emotion: Überraschung"
comment(ds$SD47_01) = "Emotion: Freude"
comment(ds$SD47_02) = "Emotion: Traurigkeit"
comment(ds$SD47_03) = "Emotion: Zorn"
comment(ds$SD47_04) = "Emotion: Angst"
comment(ds$SD47_05) = "Emotion: Ekel"
comment(ds$SD47_06) = "Emotion: Überraschung"
comment(ds$SD48_01) = "Emotion: Freude"
comment(ds$SD48_02) = "Emotion: Traurigkeit"
comment(ds$SD48_03) = "Emotion: Zorn"
comment(ds$SD48_04) = "Emotion: Angst"
comment(ds$SD48_05) = "Emotion: Ekel"
comment(ds$SD48_06) = "Emotion: Überraschung"
comment(ds$SD49_01) = "Emotion: Freude"
comment(ds$SD49_02) = "Emotion: Traurigkeit"
comment(ds$SD49_03) = "Emotion: Zorn"
comment(ds$SD49_04) = "Emotion: Angst"
comment(ds$SD49_05) = "Emotion: Ekel"
comment(ds$SD49_06) = "Emotion: Überraschung"
comment(ds$SD50_01) = "Emotion: Freude"
comment(ds$SD50_02) = "Emotion: Traurigkeit"
comment(ds$SD50_03) = "Emotion: Zorn"
comment(ds$SD50_04) = "Emotion: Angst"
comment(ds$SD50_05) = "Emotion: Ekel"
comment(ds$SD50_06) = "Emotion: Überraschung"
comment(ds$SD51_01) = "Emotion: Freude"
comment(ds$SD51_02) = "Emotion: Traurigkeit"
comment(ds$SD51_03) = "Emotion: Zorn"
comment(ds$SD51_04) = "Emotion: Angst"
comment(ds$SD51_05) = "Emotion: Ekel"
comment(ds$SD51_06) = "Emotion: Überraschung"
comment(ds$SD52_01) = "Emotion: Freude"
comment(ds$SD52_02) = "Emotion: Traurigkeit"
comment(ds$SD52_03) = "Emotion: Zorn"
comment(ds$SD52_04) = "Emotion: Angst"
comment(ds$SD52_05) = "Emotion: Ekel"
comment(ds$SD52_06) = "Emotion: Überraschung"
comment(ds$SD53_01) = "Emotion: Freude"
comment(ds$SD53_02) = "Emotion: Traurigkeit"
comment(ds$SD53_03) = "Emotion: Zorn"
comment(ds$SD53_04) = "Emotion: Angst"
comment(ds$SD53_05) = "Emotion: Ekel"
comment(ds$SD53_06) = "Emotion: Überraschung"
comment(ds$SD54_01) = "Emotion: Freude"
comment(ds$SD54_02) = "Emotion: Traurigkeit"
comment(ds$SD54_03) = "Emotion: Zorn"
comment(ds$SD54_04) = "Emotion: Angst"
comment(ds$SD54_05) = "Emotion: Ekel"
comment(ds$SD54_06) = "Emotion: Überraschung"
comment(ds$SD60_01) = "Emotion: Freude"
comment(ds$SD60_02) = "Emotion: Traurigkeit"
comment(ds$SD60_03) = "Emotion: Zorn"
comment(ds$SD60_04) = "Emotion: Angst"
comment(ds$SD60_05) = "Emotion: Ekel"
comment(ds$SD60_06) = "Emotion: Überraschung"
comment(ds$SD61_01) = "Emotion: Freude"
comment(ds$SD61_02) = "Emotion: Traurigkeit"
comment(ds$SD61_03) = "Emotion: Zorn"
comment(ds$SD61_04) = "Emotion: Angst"
comment(ds$SD61_05) = "Emotion: Ekel"
comment(ds$SD61_06) = "Emotion: Überraschung"
comment(ds$SD62_01) = "Emotion: Freude"
comment(ds$SD62_02) = "Emotion: Traurigkeit"
comment(ds$SD62_03) = "Emotion: Zorn"
comment(ds$SD62_04) = "Emotion: Angst"
comment(ds$SD62_05) = "Emotion: Ekel"
comment(ds$SD62_06) = "Emotion: Überraschung"
comment(ds$SD63_01) = "Emotion: Freude"
comment(ds$SD63_02) = "Emotion: Traurigkeit"
comment(ds$SD63_03) = "Emotion: Zorn"
comment(ds$SD63_04) = "Emotion: Angst"
comment(ds$SD63_05) = "Emotion: Ekel"
comment(ds$SD63_06) = "Emotion: Überraschung"
comment(ds$SD64_01) = "Emotion: Freude"
comment(ds$SD64_02) = "Emotion: Traurigkeit"
comment(ds$SD64_03) = "Emotion: Zorn"
comment(ds$SD64_04) = "Emotion: Angst"
comment(ds$SD64_05) = "Emotion: Ekel"
comment(ds$SD64_06) = "Emotion: Überraschung"
comment(ds$SD55_01) = "Emotion: Freude"
comment(ds$SD55_02) = "Emotion: Traurigkeit"
comment(ds$SD55_03) = "Emotion: Zorn"
comment(ds$SD55_04) = "Emotion: Angst"
comment(ds$SD55_05) = "Emotion: Ekel"
comment(ds$SD55_06) = "Emotion: Überraschung"
comment(ds$SD56_01) = "Emotion: Freude"
comment(ds$SD56_02) = "Emotion: Traurigkeit"
comment(ds$SD56_03) = "Emotion: Zorn"
comment(ds$SD56_04) = "Emotion: Angst"
comment(ds$SD56_05) = "Emotion: Ekel"
comment(ds$SD56_06) = "Emotion: Überraschung"
comment(ds$SD57_01) = "Emotion: Freude"
comment(ds$SD57_02) = "Emotion: Traurigkeit"
comment(ds$SD57_03) = "Emotion: Zorn"
comment(ds$SD57_04) = "Emotion: Angst"
comment(ds$SD57_05) = "Emotion: Ekel"
comment(ds$SD57_06) = "Emotion: Überraschung"
comment(ds$SD29_01) = "Studiengang: [01]"
comment(ds$SD30_01) = "VP: [01]"
comment(ds$SD32) = "Copyright"
comment(ds$SD65) = "Datenschutz und Freiwilligkeit"
comment(ds$TIME001) = "Verweildauer Seite 1"
comment(ds$TIME002) = "Verweildauer Seite 2"
comment(ds$TIME003) = "Verweildauer Seite 3"
comment(ds$TIME004) = "Verweildauer Seite 4"
comment(ds$TIME005) = "Verweildauer Seite 5"
comment(ds$TIME006) = "Verweildauer Seite 6"
comment(ds$TIME007) = "Verweildauer Seite 7"
comment(ds$TIME008) = "Verweildauer Seite 8"
comment(ds$TIME009) = "Verweildauer Seite 9"
comment(ds$TIME010) = "Verweildauer Seite 10"
comment(ds$TIME011) = "Verweildauer Seite 11"
comment(ds$TIME012) = "Verweildauer Seite 12"
comment(ds$TIME013) = "Verweildauer Seite 13"
comment(ds$TIME014) = "Verweildauer Seite 14"
comment(ds$TIME015) = "Verweildauer Seite 15"
comment(ds$TIME016) = "Verweildauer Seite 16"
comment(ds$TIME017) = "Verweildauer Seite 17"
comment(ds$TIME018) = "Verweildauer Seite 18"
comment(ds$TIME019) = "Verweildauer Seite 19"
comment(ds$TIME020) = "Verweildauer Seite 20"
comment(ds$TIME021) = "Verweildauer Seite 21"
comment(ds$TIME022) = "Verweildauer Seite 22"
comment(ds$TIME023) = "Verweildauer Seite 23"
comment(ds$TIME024) = "Verweildauer Seite 24"
comment(ds$TIME025) = "Verweildauer Seite 25"
comment(ds$TIME026) = "Verweildauer Seite 26"
comment(ds$TIME027) = "Verweildauer Seite 27"
comment(ds$TIME028) = "Verweildauer Seite 28"
comment(ds$TIME029) = "Verweildauer Seite 29"
comment(ds$TIME030) = "Verweildauer Seite 30"
comment(ds$TIME031) = "Verweildauer Seite 31"
comment(ds$TIME032) = "Verweildauer Seite 32"
comment(ds$TIME033) = "Verweildauer Seite 33"
comment(ds$TIME034) = "Verweildauer Seite 34"
comment(ds$TIME035) = "Verweildauer Seite 35"
comment(ds$TIME036) = "Verweildauer Seite 36"
comment(ds$TIME037) = "Verweildauer Seite 37"
comment(ds$TIME038) = "Verweildauer Seite 38"
comment(ds$TIME039) = "Verweildauer Seite 39"
comment(ds$TIME040) = "Verweildauer Seite 40"
comment(ds$TIME041) = "Verweildauer Seite 41"
comment(ds$TIME042) = "Verweildauer Seite 42"
comment(ds$TIME043) = "Verweildauer Seite 43"
comment(ds$TIME044) = "Verweildauer Seite 44"
comment(ds$TIME045) = "Verweildauer Seite 45"
comment(ds$TIME046) = "Verweildauer Seite 46"
comment(ds$TIME_SUM) = "Verweildauer gesamt (ohne Ausreißer)"
comment(ds$MAILSENT) = "Versandzeitpunkt der Einladungsmail (nur für nicht-anonyme Adressaten)"
comment(ds$LASTDATA) = "Zeitpunkt als der Datensatz das letzte mal geändert wurde"
comment(ds$FINISHED) = "Wurde die Befragung abgeschlossen (letzte Seite erreicht)?"
comment(ds$Q_VIEWER) = "Hat der Teilnehmer den Fragebogen nur angesehen, ohne die Pflichtfragen zu beantworten?"
comment(ds$LASTPAGE) = "Seite, die der Teilnehmer zuletzt bearbeitet hat"
comment(ds$MAXPAGE) = "Letzte Seite, die im Fragebogen bearbeitet wurde"
comment(ds$MISSING) = "Anteil fehlender Antworten in Prozent"
comment(ds$MISSREL) = "Anteil fehlender Antworten (gewichtet nach Relevanz)"
comment(ds$TIME_RSI) = "Maluspunkte für schnelles Ausfüllen"
comment(ds$DEG_TIME) = "Maluspunkte für schnelles Ausfüllen"



# Assure that the comments are retained in subsets
as.data.frame.avector = as.data.frame.vector
`[.avector` <- function(x,i,...) {
  r <- NextMethod("[")
  mostattributes(r) <- attributes(x)
  r
}
ds_tmp = data.frame(
  lapply(ds, function(x) {
    structure( x, class = c("avector", class(x) ) )
  } )
)
mostattributes(ds_tmp) = attributes(ds)
ds = ds_tmp
rm(ds_tmp)


ds = ds %>% select(-starts_with("TIME"))

ds %>% select(vp = SD30_01,
              sex = SD01,
              age = SD02_01,
              education  = SD10,
              occupation = SD14,
              hearImpair = SD20,
              seeImpair  = SD24,
              concentration = SD23,
              remarks      = SD18_01,
              studyAims    = SD19_01)

# unvollständig
nms = ds %>% 
      select("SD35_01":"SD57_06") %>%
      names() %>%
      str_sub(., 3, 4) %>%
      as.numeric() %>%
      unique()


