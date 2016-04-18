import re, string, nltk, json
#nltk.download() #Only ran once to download stopwords under Corpora tab
from nltk.corpus import stopwords

stop_words = stopwords.words("english") 

#this function will remove stop words, some punctuation, and separates any camelcase or number/word-word/number pairs
def transformString(s):
    output=s.decode('utf-8','ignore').encode("utf-8")
    output = ' '.join([word for word in s.split() if word not in stop_words]) #remove stopwords
    output=output.translate(None, '",()\:;') #remove some punctuation
    output=re.sub(r'\.(?!\d)', '', output) #remove any periods not followed by a digit (non-decimal periods)
    output=re.sub(r'(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])', r' \g<0>', output) #separate camelcase
    output=re.sub(r'(?<=[a-zA-Z])(?=[0-9])|(?<=[0-9])(?=[a-zA-Z])', r' \g<0>', output) #separate numbers and words
    output=re.sub(r'(\d+)(-)([a-zA-Z]+)', r'\g<1> \g<3>', output) #remove dashes between numbers and words
    output=re.sub(r'([a-zA-Z]+)(-)([a-zA-Z]+)', r'\g<1> \g<3>', output) #remover intraword dashes
    output=output.decode('utf-8').replace(u'\u00b0', ' degrees').encode("utf-8") #replace unicode symbol with word degrees
    #combine differing variation of measurements to one
    output=re.sub(r"(degrees|degree)", r"deg", output)
    output=re.sub(r"(inches|inch|in)", r"in", output)
    output=re.sub(r"(foot|feet|ft)", r"ft", output)
    output=re.sub(r"(pounds|pound|lbs|lb)", r"lb", output)
    output=re.sub(r"(square|sq)", r"sq", output)
    output=re.sub(r"(cubic|cu)", r"cu", output)
    output=re.sub(r"(gallons|gallon|gal)", r"gal", output)
    output=re.sub(r"(ounces|ounce|oz)", r"oz", output)
    output=re.sub(r"(centimeters|cm)", r"cm", output)
    output=re.sub(r"(millimeters|mm)", r"mm", output)
    output=re.sub(r"(volts|volt)", r"volt", output)
    output=re.sub(r"(watts|watt)", r"watt", output)
    output=re.sub(r"(amperes|ampere|amps|amp)", r"amp", output)
    output=output.decode('utf-8').lower() #make lowercase
    return output;

attributes = [] 
with open("pipe_separated_attributes") as a:
    next(a) #skip column header line
    for line in a:
        attributes_list = line.rstrip('\r\n').split("|") #split string from current line in the file
        for index, i in enumerate(attributes_list):
            attributes_list[index] = transformString(attributes_list[index]) 
        attributes.append(attributes_list) #add attributes to attributes list       
attributes.sort(key=lambda elem: int(elem[0]), reverse=True) #sort list by product_uid

with open("pipe_separated_prod_descrip") as f:
    next(f) #skip column header line
    for line in f:
        product_description = line.rstrip('\r\n').split("|") #strip line feeds and split string from current line in the file
        #create product dictionary and store info
        product = {}
        product['product_uid'] = int(product_description[0]) #product id
        product['product_details'] = transformString(product_description[1])
        #initialize empty string
        details=""
        brand=""
        attributes_data=""
        for a in reversed(attributes):
            if(int(a[0]) == product['product_uid']): #if attribute corresponds to current product
                if "bullet" in a[1]: #if key contains the word bullet
                    if(details==""): #concatenate to empty string if it's the first 
                        details=a[2]
                    else: #concatenate to details string  
                        details=details + " " + a[2]
                elif "brand" in a[1]: #if product has brand attribute
                    brand=a[2]
                else: #concatenate other attribute names
                    #look for values that are yes/no types 
                    if(attributes_data==""): 
                        if "yes" in a[2]: #if yes, concatenate attribute "key" words
                            attributes_data=a[1]
                        elif "no" in a[2]: 
                            attributes_data="" #if no, keep empty string
                        else:    
                            attributes_data=a[2]
                    else:
                        if "yes" in a[2]: #if yes, concatenate attribute "key" words
                            attributes_data=attributes_data + " " + a[1]
                        elif "no" in a[2]: #if no, don't add anything
                            attributes_data=attributes_data
                        else:    
                            attributes_data=attributes_data + " " + a[2]
                attributes.pop() #shorten attributes list so there is less to go through for next product
            if(int(a[0]) > product['product_uid']):
                break
        #concatenate details and attribute data to product details
        if(details!=""):
            product['product_details']=product['product_details'] + " " + details
        if(attributes_data!=""):
            product['product_details']=product['product_details'] + " " + attributes_data 
        #store brand of product
        if(brand!=""):
            product['product_brand']=brand
       
        #json_r = json.dumps(product,sort_keys=True,indent=4, separators=(',', ': '))
        #print(json_r)

        with open('product_data_cleaned.txt', "a") as json_file:
            #for data in data_iterable:
            json_file.write("{},\n".format(json.dumps(product)))


