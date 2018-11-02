
library(gemtc)
library(networkD3)
library(igraph)
library(visNetwork)
library(dplyr)
library(shiny)
library(shinythemes)
library(rsconnect)




  
          inputdata.df <- read.csv(file="C:/Users/dkratochvil/Documents/Projects/16198/temp.csv", header=TRUE, sep=",")

              #Reformatting data
              #Create a network & use the summary function output to retrieve connections as input for visualization
              network.li <- mtc.network(inputdata.df, treatments=sort(unique(inputdata.df$treatment)))
              help.li <- summary(network.li)
              
                  #fixes count of self connections
              
                  testfun<-function (x){
                    #if the number of times a treatment appears equals the number of unique studies containing that treatment, then there are no self connections, otherwise we must calculate the number of self connections
                    if(as.numeric(count(test[test$treatment==x,]))!=as.numeric(count(unique(test[test$treatment==x,])))){
                      #counter of the number of studies that appear more than twice in the list of studies containing a particular treatment
                      #we resewt the counter for each treatment (x) that we loop through
                      extraArms<-0
                      #loop through the unique studies containing a given treatment
                      for(k in unique(test[test$treatment==x,]$study)){
                        #if the study has 3 or more of the same arm, we add the number of extra arms to the counter
                        #ex. a study w/ 3 of the same arms would add 1 to the counter, a study with 4 of the same will add 2 to the counter
                        #studies with 1 or 2 arms containing the Tx of interest are not added to the counter
                        extraArms <- ifelse(as.numeric(count(test[test$study==k & test$treatment==x,]))>2,
                                            extraArms + (as.numeric(count(test[test$study==k,]))-2), 
                                            extraArms)
                  
                      }
                      #the number of self connections for a given Tx is the number of times the Tx appears minus the number of unique studies containing that study (avoid double counting) minus the number of times a study appears more than 2 times (to avoid triple or quadruple counting)
                      as.numeric(count(test[test$treatment==x,]))-as.numeric(count(unique(test[test$treatment==x,])))-extraArms
                    }
                  }

                      
                      numlist<-help.li$`Studies per treatment comparison`
                      #list of studies and their arms, long format
                      test <- network.li$data.ab[,c("study","treatment")]
                      #apply our function to test the number of self connections for all Txs; organize the results in a dataframe
                      testresults <- sapply(network.li$treatments[,1],testfun)
                      testresults2 <- as.data.frame(cbind(levels(network.li$treatments[,1]),testresults))
                        #organize our results
                        names(testresults2) <- c("tx","num")
                        numlist$t1 <- as.character(numlist$t1)
                        numlist$t2 <- as.character(numlist$t2)
                        testresults2$tx <- as.character(testresults2$tx)
                        testresults2$num <- as.numeric(as.character(testresults2$num))
                      #combine our corrected self-connection data with the data for all connections
                      for(i in 1:length(numlist$t1)){
                        if(numlist$t1[i]==numlist$t2[i]){
                          for(j in 1:length(testresults2$tx)){
                            if(testresults2$tx[j]==numlist[i,1]){
                              numlist[i,3]<- testresults2$num[j]
                            }
                          }
                        }
                      }
              
              input1.df <- numlist
              #Create input 1 for visualization & rename columns
              input1.df = rename(input1.df, from = t1, to = t2, weight = nr)
              #Create input 2 for visualization
              input2.df = inputdata.df %>% distinct(treatment)
              input2.df = rename(input2.df, id = treatment)
              
              #Calculate node weights
              sumtable.df <- input1.df %>% group_by(from) %>% summarise(strength = sum(weight))
              sumtable.df = rename(sumtable.df, id = from)
              input2.df = left_join(input2.df, sumtable.df, by = NULL, copy = FALSE)
              input2.df <- input2.df %>% mutate(strength = ifelse(is.na(strength),1,strength))
    
              #Creating interactive network visualization
              input2.df$shape <- "dot"  
              input2.df$shadow <- TRUE # Nodes will drop shadow
              input2.df$title <- input2.df$id # Text on click
              input2.df$label <- input2.df$id # Node label
              input2.df$size <- NULL
              input2.df$borderWidth <- 2 # Node border width
              input2.df$color.background <- NULL
              input2.df$color.border <- "black"
              input2.df$color.highlight.background <- "orange"
              input2.df$color.highlight.border <- "darkred"
              
              input1.df$width <- input1.df$weight # line width
              input1.df$color <- "gray"    # line color  
              input1.df$arrows <- NULL
              input1.df$smooth <- FALSE    # should the edges be curved?
              input1.df$shadow <- FALSE    # edge shadow
              input1.df$label <- input1.df$weight
              
              visNetwork(input2.df, input1.df, width="100%", height="650px", main="Treatment comparison network")%>%
                visIgraphLayout(layout = "layout_in_circle") %>%
                #visIgraphLayout(layout = "layout_with_sugiyama") %>%
                visOptions(highlightNearest = list(enabled = T, hover = T), 
                           nodesIdSelection = T)
    
 



