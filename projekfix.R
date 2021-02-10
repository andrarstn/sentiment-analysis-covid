library(shiny)
library(here)
library(vroom)
library(syuzhet)



library(plotly)
library(xml2)
library(rvest)
library(NLP)
library(tm)
library(e1071)
library(naivebayes)

library(tidymodels)
library(wordcloud)


twitter<- vroom(here("tweetclean-tidy-fix.csv"), delim = ",")


review <-as.character(twitter$text)
s<-get_nrc_sentiment(review)
klas <- data.frame(negatif=s$negative,positif=s$positive)
klasifikasi <- mutate(klas, sentimen = ifelse((klas$negatif != klas$positif),ifelse(klas$negatif!=0,print("negatif"),print("positif")),print("netral")))
#digabung
data <- data.frame(text=twitter$text,tag=klasifikasi$sentimen)

#write.csv(data,file = 'C:/Users/SIDAM/Documents/data-raw/tweetclean1.csv')


twittertrain <- filter(data, tag=="positif" | tag=="negatif")
twittertesting <- filter(data, tag=="netral")

berita<-twittertrain
tanya<-twittertesting

#write.csv(twittertrain,file = 'C:/Users/SIDAM/Documents/data-raw/tweetclean500.csv')

#-----------training
#buat semacam database kamus untuk data berita yang diberikan
#-------------mengolah text


beritacorpus <- VCorpus(VectorSource(berita$text))

beritaDTM <-DocumentTermMatrix(beritacorpus,control=list(tolower=TRUE,
                                                       removeNumbers=TRUE,
                                                       stopwords=TRUE,
                                                       removePunctuation=TRUE,
                                                       stemming=FALSE))


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
#proses testing
#------------------------------
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
hasil1<-predict(beritaklas,tanyatest,type="raw")


hasilanalisis<-data.frame(tanya$text,hasil1,hasil)




ui <- fluidPage(
  title = "_________________________",
  headerPanel("COvid"),
 
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Scatterplot", plotOutput("scatterplot")), 
                  # Plot
                  tabPanel("Data ", DT::dataTableOutput('asli')), # Output Data Dalam Tabel
                  tabPanel("Data training", DT::dataTableOutput('training')), # Output Data Dalam Tabel
                  tabPanel("Data testing", DT::dataTableOutput('testing')), # Output Data Dalam Tabel
                  tabPanel("Hasil", plotOutput("hasil")), 
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
   

    DT::datatable(hasilanalisis, options = list(lengthChange = FALSE))
  })
  
  #scatterplot
  output$scatterplot <- renderPlot({
    review_combine<-cbind(twitter$text,s)
    par(mar=rep(3,4))
    barplot(colSums(s),col=rainbow(10),ylab='count',main='sentiment analisis')
  }, height=400)
  
  #scatterplot
  output$hasil <- renderPlot({
    
    dat<-data %>% group_by(tag) %>% count(tag)
    dat<- mutate(dat,hasil="sebelum")
    has<-data.frame(text=hasilanalisis$tanya.text,tag=hasilanalisis$hasil)
    new <- rbind(twittertrain, has)
    dat1<-new %>% group_by(tag) %>% count(tag)
    dat1<- mutate(dat1,hasil="sesudah")
    dat1 <- rbind(dat, dat1)
    ggplot(dat1, aes(x=factor(dat1$hasil),y=dat1$n,fill=dat1$tag))+geom_bar(stat="identity",position=position_dodge()) + labs(y="Jumlah",x="hasil",fill="sentimen")                     
    
 }, height=400)
  
  #wordcloud
  output$Wordcloud <- renderPlot({
    
    
    dokumen <- VCorpus(VectorSource(twitter)) 
    

    dokkudtm <- TermDocumentMatrix(dokumen) 
    em <- as.matrix(dokkudtm)
    ve <- sort(rowSums(em),decreasing=TRUE)
    
    
    de <- data.frame(word = names(ve),freq=ve) 

    
    wordcloud(words = de$word, freq = de$freq, min.freq = 1,           
              max.words=50, random.order=FALSE, rot.per=0.35,            
              colors=brewer.pal(8, "Dark2"))
    
    #Misal ingin diketahui asosiasi kata dari kata-kata yang sering muncul
    #vee<-as.list(findAssocs(dokkudtm, terms =c("halal", "jawa"), corlimit = c(0.15,0.15,0.15,0.15,0.15,0.15)))

    
  })
}


shinyApp(ui = ui, server = server, options = list(height = "500px"))


