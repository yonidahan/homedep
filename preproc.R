
#Preprocessing text

preproc<-function(x){
        x<-iconv(enc2utf8(x),sub="byte")
        x<-tolower(x)
        
        x<-gsub("-"," ",x)
        x<-gsub("/"," ",x)
        x<-gsub(","," ",x)
        x<-gsub("\\)|\\("," ",x)
        x<-gsub(";"," ",x)
        x<-gsub("?"," ",x,fixed=T)
        x<-gsub("!"," ",x,fixed=T)
        x<-gsub("([[:alpha:]]+)[']","\\1",x)#he's but not 10'
        x<-gsub("\\#"," ",x)
        x<-gsub("(\\$)([[:digit:]]+)","\\1 \\2",x)#$10 ->$ 10
        x<-gsub("([[:digit:]]+)(\\$)","\\1 \\2",x)#10$ ->10 $
        x<-gsub("([[:digit:]]+)(x)(\\$)","\\1 \\2 \\3",x)#4x10$ -> 4 x 10 $
        x<-gsub("([[:alnum:]]+)(\\&)([[:alnum:]]+)","\\1 \\2 \\3",x)
        x<-gsub("([[:digit:]]+)[[:space:]]*(x)[[:space:]]*([[:digit:]]+)","\\1 \\2 \\3",x)#5x50 etc-->5 50
        x<-gsub("([[:digit:]]+)([[:alpha:]]+)","\\1 \\2",x)#50wood-->50 wood
        x<-gsub("([[:alpha:]]+)([[:digit:]]+)","\\1 \\2",x)#wood50 -->wood 50
        
        x<-gsub("([[:digit:]]+)[[:space:]]*inch[e]?[s]?","\\1 inches",x)
        x<-gsub("([[:digit:]]+)[[:space:]]*(in\\.)","\\1 inches",x)#in. -->inches
        x<-gsub("([[:digit:]]+)[[:space:]]*\\''","\\1 inches ",x)#abbrev '' --> inches
        x<-gsub("([[:digit:]]+)[[:space:]]*\\\"","\\1 inches ",x)#abbrev '' --> inches
        
        x<-gsub("([[:digit:]]+)[[:space:]]*in([[:space:]]+|$)","\\1 inches\\2",x)#10 in --> 10 inches
        
        x<-gsub("([[:digit:]]+)[[:space:]]*ft\\.","\\1 feet",x)#10 ft.-->10 feet
        x<-gsub("([[:digit:]]+)[[:space:]]*ft","\\1 feet",x)#10 ft-->10 feet
        x<-gsub("([[:digit:]]+)[[:space:]]*foot","\\1 feet",x)#foot -->feet (same info)
        x<-gsub("([[:digit:]]+)[[:space:]]*\\'","\\1 feet ",x)#abbrev 10' -->10 feet
        
        x<-gsub("([[:digit:]]+)[[:space:]]*lb[s]?[\\.]?","\\1 pounds",x)#lb,lbs -->pounds
        x<-gsub("([[:digit:]]+)[[:space:]]*pound[s]?","\\1 pounds",x)
        
        x<-gsub("([[:digit:]]+)[[:space:]]*(sq|square)[\\.]?[[:space:]]*(ft|feet)[\\.]?",
                "\\1 square foot",x)#sq ft etc -->square foot
        x<-gsub("([[:digit:]]+)[[:space:]]*(cu|cubic)[\\.]?[[:space:]]*(ft|feet|foot)[\\.]?",
                "\\1 cubic foot",x)#cu ft --> cubic foot
        
        x<-gsub("([[:digit:]]+)[[:space:]]*(gallon|gallons)[\\.]?",   #space at end? 20 gallery??
                "\\1 gallons",x)#20gal. etc -->20gallons
        x<-gsub("([[:digit:]]+)[[:space:]]*gal([[:space:]]+|$)","\\1 gallons\\2",x)
        
        x<-gsub("([[:digit:]]+)[[:space:]]*(ounces|oz|ounce)[\\.]?",
                "\\1 ounces",x)#oz. et --> ounces
        
        x<-gsub("([[:digit:]]+)[[:space:]]*(centimeters|cm)[\\.]?",
                "\\1 centimeters",x)
        
        x<-gsub("([[:digit:]]+)[[:space:]]*(milimeters|mm)[\\.]?",
                "\\1 milimeters",x)
        
        x<-gsub("([[:digit:]]+)[[:space:]]*(deg|degree|degrees)[\\.]?",
                "\\1 degrees",x)
        
        x<-gsub("([[:digit:]]+)[[:space:]]*(volt|volts)[\\.]?",
                "\\1 volts ",x)
        x<-gsub("([[:digit:]]+)[[:space:]]*v([[:space:]]+|$)","\\1 volts\\2",x)
        
        x<-gsub("([[:digit:]]+)[[:space:]]*(wat[t]?|watts)[\\.]?[[:space:]]+",
                "\\1 watts ",x)
        x<-gsub("([[:digit:]]+)[[:space:]]*w([[:space:]]+|$)","\\1 watts\\2",x)
        
        x<-gsub("([[:digit:]]+)[[:space:]]*(amp|ampere|amperes|amps)[\\.]?[[:space:]]+",
                "\\1 amperes ",x)
        x<-gsub("([[:digit:]]+)[[:space:]]*a([[:space:]]+|$)","\\1 amperes\\2",x)
        
        x<-gsub("[\\.]"," ",x) ## ??!! check again !!??
                
        x<-gsub("([[:digit:]]+)([[:alpha:]]+)","\\1 \\2",x)## ??!! check again !!?? --> r30 = r 30 
        x<-gsub("([[:alpha:]]+)([[:digit:]]+)","\\1 \\2",x)
        
        x<-gsub("\\:"," ",x)
        
        x<-gsub("b&d","black & decker",x)
        
        x<-gsub("\\&"," ",x)
        x<-gsub("\\="," ",x)
        x<-gsub("\\+"," ",x)
        
        #Misspellings
        x<-gsub("batteries","battery",x)
        x<-gsub("dack|decking","deck",x)
        x<-gsub("black decke[r]?","black decker",x)
        
        x<-gsub("([[:alpha:]]+)(kitchen)","\\1 \\2",x)#faucetkitchen
        x<-gsub("daucet|faucets|daucets","faucet",x)
        
        x<-gsub("dleta","delta",x)
        
        x<-gsub("(power)([[:alnum:]]+)","\\1 \\2",x)
        x<-gsub("([[:alnum:]]+)(power)","\\1 \\2",x)
        
        x<-gsub("hoses","hose",x)
        
        x<-gsub("\\:"," ",x)
        
        x<-gsub("auqatic","aquatic",x)
        x<-gsub("mircowave","microwave",x)
        
        x<-gsub("ectric","electric",x)
        x<-gsub("electic","electric",x)
        x<-gsub("(electric)([[:alnum:]]+)","\\1 \\2",x)
        
        x<-gsub("(pak|packs|paks)","pack",x)
        x<-gsub("galv(e|a)ni(z|s)e(d)?","galvanized",x)
        x<-gsub("flouriscent|florisant|florisent|flurorescent","fluorescent",x)
        x<-gsub("extirior","exterior",x)
        x<-gsub("blcktop","blacktop",x)
        x<-gsub("(switch)([[:alnum:]]+)","\\1 \\2",x)#switchwhite
        x<-gsub("(alu)([[:alpha:]]+)","aluminium",x)#alumium
        x<-gsub("eppoxy","epoxy",x)
        
        x<-gsub("(light)([[:alnum:]]+)","\\1 \\2",x)
        x<-gsub("([[:alnum:]]+)(light)","\\1 \\2",x)
        
        x<-gsub("ceadar","cedar",x)
        x<-gsub("(cedar)([[:alpha:]]+)","\\1 \\2",x)#cedarsafe
        
        x<-gsub("prop[[:alpha:]]+ane","propane",x)#propiane
        x<-gsub("([[:alpha:]]*)[[:space:]]*air[[:space:]]*condition[[:alpha:]]*",
                "\\1 air condition",x)#heatairconditioner
        x<-gsub("^ac$|^a[[:space:]]*c$","air condition",x)
        
        x<-gsub("\\\""," ",x)
        x<-gsub("\\."," ",x)
        x<-gsub("\\%"," ",x)
               
        ##Needs: safety+plus
        ##Problem: electric
        
        x<-removeWords(x,stopwords("english"))
        
        x<-PlainTextDocument(x)
        x<-stemDocument(x)
        x<-as.character(x)
        
        x<-gsub("\\s+"," ",x)
        x<-gsub("^[[:space:]]+([[:alnum:]]+)","\\1",x)

        return(x)
}



