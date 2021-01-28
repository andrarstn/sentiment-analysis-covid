library(shiny)
library(here)
library(vroom)
library(ggplot2)
library(plotly)
library(xml2)
library(rvest)
library(NLP)
library(tm)
library(e1071)
library(naivebayes)
library(dslabs)
library(tidymodels)
library(syuzhet)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(stringr)
library(dplyr)

twitter<- vroom(here("~/tweetclean-tidy-fix.csv"), delim = ",")


review <-as.character(twitter$text)
s<-get_nrc_sentiment(review)
klas <- data.frame(negatif=s$negative,positif=s$positive)
klasifikasi <- mutate(klas, sentimen = ifelse((klas$negatif != klas$positif),ifelse(klas$negatif!=0,print("negatif"),print("positif")),print("netral")))
#digabung
data <- data.frame(text=twitter$text,tag=klasifikasi$sentimen)

#write.csv(data,file = 'C:/Users/SIDAM/Documents/data-raw/tweetclean1.csv')


twittertrain <- filter(data, tag=="positif" | tag=="negatif")
twittertesting <- filter(data, tag=="netral") %>% head(100)

berita<-twittertrain
tanya<-twittertesting

#write.csv(twittertrain,file = 'C:/Users/SIDAM/Documents/data-raw/tweetclean500.csv')

#-----------training
#buat semacam database kamus untuk data berita yang diberikan
#-------------mengolah text
beritacorpus<-VCorpus(VectorSource(berita$text))
#beritacorpus[[4]]$content
#buat Matrix Kata Dokumen atau Document Term Matrix
beritaDTM<-DocumentTermMatrix(beritacorpus,control=list(tolower=TRUE,
                                                        removeNumbers=TRUE,
                                                        stopwords=TRUE,
                                                        removePunctuation=TRUE,
                                                        stemming=TRUE))
#inspect(beritaDTM)
#kemudian jika ingin dilihat kata apa saja yang didapatkan gunakan 
#beritaDTM$dimnames$Terms
beritafreq<-findFreqTerms(beritaDTM,1)
beritaDTMfreq<-beritaDTM[,beritafreq]
#fungsi mengubah 0,1
convert_counts<-function(x){x<-ifelse(x>0,"yes","no")}
beritatrain<-apply(beritaDTMfreq,MARGIN=2,convert_counts)
#beritatrain
#untuk kategorisasinya/tag kita buat sebagai faktor,
#---------------mengolah tag
beritatag <- factor(berita$tag)

#Selanjutnya kita latih data training tersebut dengan Naive Bayes
beritaklas<-naiveBayes(beritatrain,beritatag,laplace=1)
#beritaklas
#---------------------------------

ui <- fluidPage(
  title = "_________________________",
  headerPanel("Covid"),
 
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Scatterplot", plotOutput("scatterplot")), 
                  # Plot
                  tabPanel("Data ", DT::dataTableOutput('asli')), # Output Data Dalam Tabel
                  tabPanel("Data training", DT::dataTableOutput('training')), # Output Data Dalam Tabel
                  tabPanel("Data testing", DT::dataTableOutput('testing')), # Output Data Dalam Tabel
                  tabPanel("Wordcloud", plotOutput("Wordcloud"))
      )
    )
  
)


server <- function(input, output, session) {
  # Output Data tabel
  output$asli = DT::renderDataTable({

    DT::datatable(data, options = list(lengthChange = FALSE))
  })
  output$training = DT::renderDataTable({

    DT::datatable(twittertrain, options = list(lengthChange = FALSE))
  })
  output$testing = DT::renderDataTable({
    #-----------testing
    
    
    tanyacorpus <- VCorpus(VectorSource(tanya$text))
    #kemudian dibuat juga Matriks Kata Dokumen (DTM)
    tanyaDTM <-DocumentTermMatrix(tanyacorpus,control=list(tolower=TRUE,
                                                           removeNumbers=TRUE,
                                                           stopwords=TRUE,
                                                           removePunctuation=TRUE,
                                                           stemming=FALSE))
    #kemudian disusun per satu frekuensi kemunculan tiap kata.
    tanyafreq<-findFreqTerms(tanyaDTM,1)
    
    tanyaDTMfreq <- tanyaDTM[,tanyafreq]
    
    convert_counts<-function(y){y<-ifelse(y>0,"yes","no")}
    tanyatest<-apply(tanyaDTMfreq,MARGIN=2,convert_counts)
    
    #kemudian diprediksikan kata-kata tersebut
    hasil<-predict(beritaklas,tanyatest,type="class")
    
    #ika ingin diketahui hasil prediksinya berupa nilai probabilitas
    hasil<-predict(beritaklas,tanyatest,type="raw")
    
    
    hasilanalisis<-data.frame(tanya$text,hasil)

    DT::datatable(hasilanalisis, options = list(lengthChange = FALSE))
  })
  
  #scatterplot
  output$scatterplot <- renderPlot({
    review_combine<-cbind(twitter$text,s)
    par(mar=rep(3,4))
    barplot(colSums(s),col=rainbow(10),ylab='count',main='sentiment analisis')
  }, height=400)
  
  #wordcloud
  output$Wordcloud <- renderPlot({
    
    
    dokumen <- VCorpus(VectorSource(twitter)) 
    
    #kemudian kita buat Matriks Kata-kata
    dokumenDTM<-DocumentTermMatrix(dokumen,control=list(tolower=TRUE,
                                                        removeNumbers=TRUE,
                                                        stopwords=TRUE,
                                                        removePunctuation=TRUE,
                                                        stemming=TRUE))
    

    #kemudian bisa kita lihat semua kata-kata yang ada
    dokumenDTM$dimnames$Terms
    #melihat kata dengan frekuensi tertentu
    dokumenfreq<-findFreqTerms(dokumenDTM,3)

    
    dokkudtm <- TermDocumentMatrix(dokumen) 
    em <- as.matrix(dokkudtm)
    ve <- sort(rowSums(em),decreasing=TRUE)
    
    
    de <- data.frame(word = names(ve),freq=ve) 
    #melihat kata terbesar
   # head(de, 15)
    
    wordcloud(words = de$word, freq = de$freq, min.freq = 1,           
              max.words=50, random.order=FALSE, rot.per=0.35,            
              colors=brewer.pal(8, "Dark2"))
    
    #Misal ingin diketahui asosiasi kata dari kata-kata yang sering muncul
    #vee<-as.list(findAssocs(dokkudtm, terms =c("halal", "jawa"), corlimit = c(0.15,0.15,0.15,0.15,0.15,0.15)))

    
  })
}


shinyApp(ui = ui, server = server, options = list(height = "500px"))
