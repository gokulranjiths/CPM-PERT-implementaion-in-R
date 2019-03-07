#function that returns the position of nodes during forward pass calculation
getnodeposes<-function(n,total_activity,starting_node,ending_node)
{
 ab<-rep(NA,total_activity)
 pos<-1
 for (i in 1:total_activity)
  {
  if(ending_node[i]==n)
   { 
     ab[pos]<-c(starting_node[i])   
     pos<-pos+1
   } 
  }
  return (ab)
}
#function that returns the position of nodes during backward pass calculation
getnodeposlc<-function(n,total_activity,starting_node,ending_node)
{
 ab<-rep(NA,total_activity)
 pos<-1
 for (i in 1:total_activity)
	{
	 if(starting_node[i]==n)
	  {
	   ab[pos]<-c(ending_node[i])
 	   pos<-pos+1
	  }
	}
 return(ab)
}

#function that returns the distance between nodes during forward pass calculation
getnodedises<-function(n,total_activity,starting_node,ending_node,te)
{
 ab<-rep(NA,total_activity)
 pos<-1
 for (i in 1:total_activity)
  {
  if(ending_node[i]==n)
    { 
     ab[pos]<-c(te[i])   
     pos<-pos+1
     } 
    }
  return (ab)
}
#function that returns the distance between nodes during backward pass calculation
getnodedislc<-function(n,total_activity,starting_node,ending_node,te)
{
 ab<-rep(NA,total_activity)
 pos<-1
 for (i in 1:total_activity)
	{
	 if(starting_node[i]==n)
	  {
	   ab[pos]<-c(te[i])   
 	   pos<-pos+1
	  }
	}
 return(ab)
}


#function that returns the distance by performing forward pass calculation
fwdpasscalc<-function(a,n,total_activity,te,es,d)
{
 len<-length(d[!is.na(d)])
 if(len!=0)
 {
 maxcal<-rep(NA,len)
 for (i in 1:len)
	{
	 maxcal[i]<-es[a[i]]+d[i]
	}
 r=max(maxcal)
 return (r)
 }
 else 
 {
   return (NA)
 }
}
#function that returns the distance by performing backward pass calculation
bwdpasscalc<-function(a,n,total_activity,te,lc,d)
{
 len<-length(d[!is.na(d)])
 if(len!=0)
 {
 mincal<-rep(NA,len)
 for (i in 1:len)
	{
	 mincal[i]<-lc[a[i]]-d[i]
	}


 r<-c(min(mincal))
 return (r)
 }
 else 
 {
  return (lc[n+1])
 }
}
#function that takes only numeric values as input
readinteger <- function()
{ 
  n <- readline()
  if(!grepl("^[0-9]+$",n))
  {
    return(readinteger())
  }
  return(as.integer(n))
}
#getting the input for all the activities 
print("ENTER THE TOTAL NUMBER OF ACTIVITIES:")
total_activity<-readinteger()
starting_node<-rep(NA,total_activity)
ending_node<-rep(NA,total_activity)
#t0 means optimistic time
#tm means moderate time
#tp means pessimistic time
t0<-rep(NA,total_activity)
tm<-rep(NA,total_activity)
tp<-rep(NA,total_activity)
te<-rep(NA,total_activity)
vij<-rep(NA,total_activity)
sn<-rep(NA,total_activity)
en<-rep(NA,total_activity)
tvij<-rep(NA,total_activity)
for (i in 1:total_activity)
	{
	print(sprintf("Enter the details of activity %d:",i))
	starting_node[i]<-as.integer(readline("Starting node  :"))
	ending_node[i]<-as.integer(readline("Ending node    :"))
	t0[i]<-as.integer(readline("Optimum time   :"))
	tm[i]<-as.integer(readline("Moderate time  :"))
	tp[i]<-as.integer(readline("Pesimistic time:"))
	te[i]<-((t0[i]+(4*(tm[i]))+tp[i])/6)
	vij[i]<-(((tp[i]-t0[i])/6)^2)
	}
es<-rep(NA,total_activity)
es[1]<-0
sn[1]<-0
en[1]<-0
tvij[1]<-vij[1]
for (i in 2:total_activity)
	{
 	 a<-getnodeposes(i,total_activity,starting_node,ending_node)
	 d<-getnodedises(i,total_activity,starting_node,ending_node,te)
	 b<-fwdpasscalc(a,i,total_activity,te,es,d)
	 es[i]=b
	 if(length(a[!is.na(a)])!=0)
	 {
	  sn[i]<-c(i)
	 en[i]<-c(a[which.max(d)])
 	 tvij[i]<-c(vij[i])
	 }
	 else
	 {
	  sn[i]<-c(0)
  	  en[i]<-c(0)
	  tvij[i]<-c(vij[i])
                }
 }
es<-(es[!is.na(es)])
eslen<-length(es[!is.na(es)])
lc<-rep(NA,eslen)
lc[eslen]<-c(es[eslen])
for (i in (eslen-1):1)
	{
 	 x<-getnodeposlc(i,total_activity,starting_node,ending_node)
	 y<-getnodedislc(i,total_activity,starting_node,ending_node,te)
	 z<-bwdpasscalc(x,i,total_activity,te,lc,y)
 	 lc[i]=z
 	}
position<-1
csn<-rep(NA,total_activity)
cen<-rep(NA,total_activity)
cp<-rep(NA,total_activity)
for (i in 1:eslen)
{
  if(es[i] == lc[i])
  {
   csn[i]<-c(sn[i])
   cen[i]<-c(en[i])
   cp[i]<-c(es[i])
  }
}
csn<-(csn[!is.na(csn)])
cen<-(cen[!is.na(cen)])
csnlen<-length(csn)
for (i in 1:csnlen)
{
 if((csn[i]==0)&&(cen[i]==0))
	{
	 csn[i]<-c(NA)
	 cen[i]<-c(NA)
	 cp[i]<-c(NA)
	}
}
csn<-(csn[!is.na(csn)])
cen<-(cen[!is.na(cen)])
csnlen<-length(csn)
cp<-(cp[!is.na(cp)])
ftvij<-c(NA,total_activity)
pos<-2
for (i in 1:csnlen)
{
 for (j in 1:total_activity)
 {
   if((cen[i]==starting_node[j])&&(csn[i]==ending_node[j]))
	{
	 ftvij[pos]<-c(vij[j])
	 pos=pos+1
	}
 }
}

ftvij<-(ftvij[!is.na(ftvij)])
critical_time<-max(cp)
reducing_time<-as.integer(readline("Enter the total time to reduce:"))
sqrtftvij<-sqrt(sum(ftvij))
z<-c((reducing_time)/(sqrtftvij))
print("RESULTS:")
print("Starting node  Ending node  Critical time") 
for (i in 1:length(csn))
{
 print(sprintf("%d                %d                  %f",cen[i],csn[i],cp[i]))
}
ans<-round(100*(dnorm(z)))
print(sprintf("The probability of completing the activity atleast %f weeks before the expected time is given as %f percentage:",reducing_time,ans))
