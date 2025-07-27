# test script to evaluate independent compartments
#   of conceptual model for criterion d
## 2025-07-25

##
## 1. Set up
##



##
## 2. Groom data
##
   # set red list categories
     categories <- 
       c('LC','NT','VU','EN','CR')

   # set model compartments
     compartments <- 
       c('coral',
         'algae',
         'parrot',
         'grouper')

   # create full matrix of possibilities
     x <- 
       expand.grid(rep(list(1:5), 4))
        names(x) <- compartments
        head(x)
        nrowx <- nrow(x)

  # set matrix outcomes
    x$seq <- NA
    x$unorder <- NA
    x$max<-apply(x[, 1:4], 1, max)

 ## -- loop for outcomes -- ##
  # loop for outcome 1 - sequential compartments
    for (i in 1:nrow(x)){
  
      # set first compartment
        if(x[i,2] > x[i,1]){y <- x[i,2]}
  
    }

    # iterate through rows
      for(j in 1:nrow(x)){

        # set data
          y <- x[j,1]

        # progress through algae & fishes
          for (i in 2:4) {

            # set remaining compartments
              if(x[j,i] > y) {y <- y + 1}

          }
          
          # set sequence
            x$seq[j] <- y

      }

   # loop for outcome 2 - any order compartments
     for(j in 1:nrow(x)) {

       # set object
         y <- x[j,1]
  
       # set parameters
         z  <- NA
         z  <- x[j,c(2:4)] # create new object - exclude coral
         z1 <- z[,c(order(z))]
         # z1 <- z1[,c(1,2,3,5)]
  
    # review for other compartments 
      for(i in 1:3){

        # set data
          if(z1[i] > y) {y <- y + 1}

      }

     # set unorder  
       x$unorder[j]<-y


     }

##
## 4. Harmonise results
##

 ## -- Assess degree/types of disagreements              ##
 ##      the cases in which the three methods agree,     ##
 ##      and interpret.                                  ##
 ##    When 1 and 2 agree, but not 3, when 1 and 3       ##
 ##     agree, but not 2, and 2 and 3 agree but not 1    ##
 ##     and when 1 (our stated one is more or less       ##
 ##     conservative than the others, and why)           ##
 ##     (i.e. is less than 2 or 3)                    -- ##

  # evaluate comparisons
    x$comparison <- NA

  # loop to evaluate
    for (i in 1:nrow(x)){
      if (x$seq[i]==x$unorder[i]&x$seq[i]==x$max[i]){x$comparison[i]<-"all_three"}
      else if (x$seq[i]==x$unorder[i]&x$seq[i]!=x$max[i]){x$comparison[i]<-"seq_unord_only"}
      else if (x$seq[i]!=x$unorder[i]&x$seq[i]==x$max[i]){x$comparison[i]<-"seq_max_only"}
      else if (x$seq[i]!=x$unorder[i]&x$seq[i]!=x$max[i]&x$unorder[i]==x$max[i]){x$comparison[i]<-"max_unord_only"}
  else{x$comparison[i]<-"no_match"}
}

   # review categories
     x$comparison %>% unique() # all fall under one of the 4 cateogries detailed above

x$conservative<-NA
for (i in 1:nrow(x)){
  if (x$comparison[i]=="seq_unord_only"&x$seq[i]<x$max[i]){x$conservative[i]<-"more_conservative_than_max"}
  if (x$comparison[i]=="seq_max_only"&x$seq[i]<x$unorder[i]){x$conservative[i]<-"more_conservative_than_unorder"}
  if (x$comparison[i]=="seq_max_only"&x$seq[i]>x$unorder[i]){x$conservative[i]<-"less_conservative_than_unorder"}
  if (x$seq[i]<x$unorder[i]&x$seq[i]<x$max[i]){x$conservative[i]<-"more_conservative_than_both"}
  
  # if (x$seq[i]>x$max[i]){x$conservative[i]<-"less_conservative_than_max"} #should be impossible
  # if (x$seq[i]<x$unorder[i]&x$seq[i]==x$max){x$conservative[i]<-"more_conservative_than_unorder"}
  # if (x$seq[i]<x$max[i]){x$conservative[i]<-"more_conservative_than_max"}
  else if(x$comparison[i]=="all_three") {x$conservative[i]<-"matched"}
  else{}
}

write.csv(x,"/Users/Mishal/Documents/CORDIO/Crit_D_permutations_5_levels_4_ind.csv",row.names = F)


# Using 5 compartments ----------------------------------------------------

#Red list categories and their order
Categories <- c('LC','NT','VU','EN','CR')

#Model compartments
compartments <- c('coral','algae','parrot','grouper',"variable5")

#Full matrix of possibilities
x <- expand.grid(rep(list(1:5), 5))
names(x)<-compartments
head(x)
nrowx<-nrow(x)

x$seq<-NA
x$unorder<-NA
x$max<-apply(x[, 1:5], 1, max)

#outcome 1 - sequential compartments

for (i in 1:nrow(x)){
  
  if (x[i,2]>x[i,1]){y<-x[i,2]}
  
  
  
}

for (j in 1:nrow(x)) {
  y <- x[j,1]
  for (i in 2:5) {
    if (x[j,i] > y) {y <- y + 1}
  }
  x$seq[j]<-y
}

#outcome 2 - any order compartments
for (j in 1:nrow(x)) {
  y <- x[j,1]
  
  z<-NA
  z<-x[j,c(2:5)] #create new object - exclude coral
  z1<-z[,c(order(z))]
  # z1<-z1[,c(1,2,3,5)]
  
  
  for (i in 1:4) {
    if (z1[i] > y) {y <- y + 1}
  }
  
  x$unorder[j]<-y
}



#assess degree/types of disagreements
# the cases in which the three methods agree, and explain that

# when 1 and 2 agree, but not 3, when 1 and 3 agree, but not 2, and 2 and 3 agree but not 1

# and when 1 (our stated one is more or less conservative than the others, and why) i.e. is less than 2 or 3
x$comparison<-NA

for (i in 1:nrow(x)){
  if (x$seq[i]==x$unorder[i]&x$seq[i]==x$max[i]){x$comparison[i]<-"all_three"}
  else if (x$seq[i]==x$unorder[i]&x$seq[i]!=x$max[i]){x$comparison[i]<-"seq_unord_only"}
  else if (x$seq[i]!=x$unorder[i]&x$seq[i]==x$max[i]){x$comparison[i]<-"seq_max_only"}
  else if (x$seq[i]!=x$unorder[i]&x$seq[i]!=x$max[i]&x$unorder[i]==x$max[i]){x$comparison[i]<-"max_unord_only"}
  else{x$comparison[i]<-"no_match"}
}

unique(x$comparison) #all fall under one of the 4 cateogries detailed above

x$conservative<-NA
for (i in 1:nrow(x)){
  if (x$comparison[i]=="seq_unord_only"&x$seq[i]<x$max[i]){x$conservative[i]<-"more_conservative_than_max"}
  if (x$comparison[i]=="seq_max_only"&x$seq[i]<x$unorder[i]){x$conservative[i]<-"more_conservative_than_unorder"}
  if (x$comparison[i]=="seq_max_only"&x$seq[i]>x$unorder[i]){x$conservative[i]<-"less_conservative_than_unorder"}
  if (x$seq[i]<x$unorder[i]&x$seq[i]<x$max[i]){x$conservative[i]<-"more_conservative_than_both"}
  
  # if (x$seq[i]>x$max[i]){x$conservative[i]<-"less_conservative_than_max"} #should be impossible
  # if (x$seq[i]<x$unorder[i]&x$seq[i]==x$max){x$conservative[i]<-"more_conservative_than_unorder"}
  # if (x$seq[i]<x$max[i]){x$conservative[i]<-"more_conservative_than_max"}
  else if(x$comparison[i]=="all_three") {x$conservative[i]<-"matched"}
  else{}
}

write.csv(x,"/Users/Mishal/Documents/CORDIO/Crit_D_permutations_5_levels_5_ind.csv",row.names = F)

# Using 6 compartments ----------------------------------------------------

#Red list categories and their order
Categories <- c('LC','NT','VU','EN','CR')

#Model compartments
compartments <- c('coral','algae','parrot','grouper',"variable5","variable 6")

#Full matrix of possibilities
x <- expand.grid(rep(list(1:5), 6))
names(x)<-compartments
head(x)
nrowx<-nrow(x)

x$seq<-NA
x$unorder<-NA
x$max<-apply(x[, 1:6], 1, max)

#outcome 1 - sequential compartments

for (i in 1:nrow(x)){
  
  if (x[i,2]>x[i,1]){y<-x[i,2]}
  
  
  
}

for (j in 1:nrow(x)) {
  y <- x[j,1]
  for (i in 2:6) {
    if (x[j,i] > y) {y <- y + 1}
  }
  x$seq[j]<-y
}

#outcome 2 - any order compartments
for (j in 1:nrow(x)) {
  y <- x[j,1]
  
  z<-NA
  z<-x[j,c(2:6)] #create new object - exclude coral
  z1<-z[,c(order(z))]
  # z1<-z1[,c(1,2,3,5)]
  
  
  for (i in 1:5) {
    if (z1[i] > y) {y <- y + 1}
  }
  
  x$unorder[j]<-y
}



#assess degree/types of disagreements
# the cases in which the three methods agree, and explain that

# when 1 and 2 agree, but not 3, when 1 and 3 agree, but not 2, and 2 and 3 agree but not 1

# and when 1 (our stated one is more or less conservative than the others, and why) i.e. is less than 2 or 3
x$comparison<-NA

for (i in 1:nrow(x)){
  if (x$seq[i]==x$unorder[i]&x$seq[i]==x$max[i]){x$comparison[i]<-"all_three"}
  else if (x$seq[i]==x$unorder[i]&x$seq[i]!=x$max[i]){x$comparison[i]<-"seq_unord_only"}
  else if (x$seq[i]!=x$unorder[i]&x$seq[i]==x$max[i]){x$comparison[i]<-"seq_max_only"}
  else if (x$seq[i]!=x$unorder[i]&x$seq[i]!=x$max[i]&x$unorder[i]==x$max[i]){x$comparison[i]<-"max_unord_only"}
  else{x$comparison[i]<-"no_match"}
}

unique(x$comparison) #all fall under one of the 4 cateogries detailed above

x$conservative<-NA
for (i in 1:nrow(x)){
  if (x$comparison[i]=="seq_unord_only"&x$seq[i]<x$max[i]){x$conservative[i]<-"more_conservative_than_max"}
  if (x$comparison[i]=="seq_max_only"&x$seq[i]<x$unorder[i]){x$conservative[i]<-"more_conservative_than_unorder"}
  if (x$comparison[i]=="seq_max_only"&x$seq[i]>x$unorder[i]){x$conservative[i]<-"less_conservative_than_unorder"}
  if (x$seq[i]<x$unorder[i]&x$seq[i]<x$max[i]){x$conservative[i]<-"more_conservative_than_both"}
  
  # if (x$seq[i]>x$max[i]){x$conservative[i]<-"less_conservative_than_max"} #should be impossible
  # if (x$seq[i]<x$unorder[i]&x$seq[i]==x$max){x$conservative[i]<-"more_conservative_than_unorder"}
  # if (x$seq[i]<x$max[i]){x$conservative[i]<-"more_conservative_than_max"}
  else if(x$comparison[i]=="all_three") {x$conservative[i]<-"matched"}
  else{}
}

##
## 5. Generate results
##
  # point to save locale
    save_locale <- "/Users/Mishal/Documents/CORDIO/"

  # export
    write.csv(x,
              paste0(save_locale, "Crit_D_permutations_5_levels_6_ind.csv",
              row.names = FALSE)

##
## 6. Clean up workspace
##

