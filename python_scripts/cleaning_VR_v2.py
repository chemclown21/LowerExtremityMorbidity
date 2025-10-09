#Import necessary dependencies
import numpy as np
import pandas as pd
import icd10
from scipy.stats import chi2_contingency

def csc(str): #comma-separated codes to list 'S32.52,S82.90' -> ['S32.52','S82.90']
    list_of_codes = []
    if not "," in str:
        list_of_codes.append(str)
    else:
        list_of_codes=str.split(',')
    return list_of_codes

def find_unique_codes(list,search_term,n = None): #n characters to include #this is just used for my dbugging & querying icds
    unique_codes = []
    for patient in range(len(list)):
        if isinstance(list[patient],str):
            #print(list[patient])
            this_patients_codes = csc(list[patient])
            for code in range(len(this_patients_codes)):
                if (not this_patients_codes[code][:n] in unique_codes) and (search_term in this_patients_codes[code]):
                    unique_codes.append(this_patients_codes[code][:n])
        else:
            continue
    return unique_codes

def print_diagnoses(list_of_codes): #this is just used for my dbugging & querying icds
    sd_list = sorted(list_of_codes)
    unique_diagnoses = []

    for i in range(len(sd_list)):
        #print(sd_unique[i])
        try:
            code = sd_list[i]
            diagnosis = icd10.find(code).description
        except:
            try:
                code = sd_list[i][:7]
                diagnosis = icd10.find(code).description
            except:
                try:
                    code = sd_list[i][:6]
                    diagnosis = icd10.find(code).description
                except:
                    try:
                        code = sd_list[i][:5]
                        diagnosis = icd10.find(code).description
                    except:
                        try:
                            code = sd_list[i][:3]
                            diagnosis = icd10.find(code).description
                        except:
                            print("cant find this code!")
        
        if not diagnosis in unique_diagnoses:
            unique_diagnoses.append(diagnosis)
            print(code,": ",diagnosis)
        
        #if not "racture" in diagnosis:
        #    print("This diagnosis doesnt have the word fracture! - ",diagnosis)
    
    #print("These are all of the unique diagnoses")
    #print(unique_diagnoses)

#try this code if you want an example of what I was doing this for. 3 means you only look at the part of codes before the period
#current = sorted(find_unique_codes(list(df['primaryecodeicd10'].unique()),"",3))
#print_diagnoses(current)

def anycomp(list): #any complications in this list of booleans?  [0 0 0 0] false [0 0 1 0] true
    N = 0
    for i in range(len(list)):
        if list[i] == 'Yes' or list[i] == 'True' or list[i] == True or list[i] == '1' or list[i] == 1:
            N+=1
        elif list[i] == 'No' or list[i] == 'False' or list[i] == False or list[i] == '0' or list[i] == 0:
            N+=0
    if N>=1:
        return True
    else:
        return False

def calculate(dataframe,factor,Ns,calculate_pvalue=False):
    row = []
    row.append(str(sum(df.loc[df["ILE"] == True][factor]))+" ("+str(round(sum(df.loc[df["ILE"] == True][factor])/Ns[0]*100,1))+"%)")
    row.append(str(sum(df.loc[df["MLE"] == True][factor]))+" ("+str(round(sum(df.loc[df["MLE"] == True][factor])/Ns[1]*100,1))+"%)")
    row.append(str(sum(df.loc[df["ULE"] == True][factor]))+" ("+str(round(sum(df.loc[df["ULE"] == True][factor])/Ns[2]*100,1))+"%)")
    if calculate_pvalue == True:
        obs = np.array([[sum(df.loc[df["ILE"] == True][factor]), 
                         sum(df.loc[df["MLE"] == True][factor]), 
                         sum(df.loc[df["ULE"] == True][factor])], [
                        Ns[0]-sum(df.loc[df["ILE"] == True][factor]), 
                        Ns[1]-sum(df.loc[df["MLE"] == True][factor]), 
                        Ns[2]-sum(df.loc[df["ULE"] == True][factor])]]) 
        pval = round(chi2_contingency(obs).pvalue,3)
        if pval == 0:
            pval = "<0.001"
        row.append(pval)
    return row

def progressbar(progress,total): #animation to see speed of processing
    percent = 100*(progress/float(total))
    bar='█'*int(percent)+'-'*(100-int(percent))
    print(f"\r|{bar}|{percent:.2f}%",end="\r")


#Categories of LE Injury
#the "sharfman_..._..." lists are names of the new columns to be added
#the LE_ and UE_ dictionaries take an icd10 prefix and direct which column to assign true for a given patient

#S32: Fracture of lumbar spine and pelvis
sharfman_LE_S32 = ['Fracture of acetabulum',                                      #S32.4 Fracture of acetabulum
                   'Fracture of pubis',                                           #S32.5 Fracture of pubis
                   'Fracture of ilium',                                           #S32.3 Fracture of ilium
                   'Fracture of ischium',                                         #S32.6 Fracture of ischium
                   'Fracture of pelvis multiple w/ disruption of pelvic circle',  #S32.81 Multiple fractures of pelvis with disruption of pelvic ring
                   'Fracture of pelvis without disruption of pelvic circle',      #S32.82 Multiple fractures of pelvis without disruption of pelvic ring
                   'Fracture of pelvis, other']                                   #S32.88 Fracture of other parts of pelvis - no desc
                                                                                  #S32.89 Fracture of other parts of pelvis
                                                                                  #S32.9  Fracture of unspecified parts of lumbosacral spine and pelvis

LE_dict =      dict.fromkeys(['S32.4']                          , 'Fracture of acetabulum')
LE_dict.update(dict.fromkeys(['S32.5']                          , 'Fracture of pubis'))
LE_dict.update(dict.fromkeys(['S32.3']                          , 'Fracture of ilium'))
LE_dict.update(dict.fromkeys(['S32.6']                          , 'Fracture of ischium'))
LE_dict.update(dict.fromkeys(['S32.81']                         , 'Fracture of pelvis multiple w/ disruption of pelvic circle'))
LE_dict.update(dict.fromkeys(['S32.82']                         , 'Fracture of pelvis without disruption of pelvic circle'))
LE_dict.update(dict.fromkeys(['S32.88','S32.89','S32.9','S32.8'], 'Fracture of pelvis, other'))


#S72: Fracture of femur
sharfman_LE_S72 = ['Fracture of femoral neck',         #S72.00 Fracture of unspecified part of neck of femur
                                                       #S72.01 Unspecified intracapsular fracture of femur
                                                       #S72.02 Fracture of epiphysis (separation) (upper) of femur
                                                       #S72.03 Midcervical fracture of femur
                                                       #S72.04 Fracture of base of neck of femur
                   'Fracture of other parts of femur'] #S72.05 Unspecified fracture of head of femur
                                                       #S72.06 Articular fracture of head of femur
                                                       #S72.08 Fracture of head and neck of femur - no desc
                                                       #S72.09 Other fracture of head and neck of femur
                                                       #S72.1  Pertrochanteric fracture
                                                       #S72.2  Subtrochanteric fracture of femur
                                                       #S72.3  Fracture of shaft of femur
                                                       #S72.4  Fracture of lower end of femur
                                                       #S72.8  Other fracture of femur
                                                       #S72.9  Unspecified fracture of femur
LE_dict.update(dict.fromkeys(['S72.00','S72.01','S72.02','S72.03','S72.04'], 'Fracture of femoral neck'))
LE_dict.update(dict.fromkeys(['S72.05','S72.06','S72.08','S72.09','S72.1',
                             'S72.2','S72.3','S72.4','S72.8','S72.9'], 'Fracture of other parts of femur'))

#S82: Fracture of lower leg, including ankle
sharfman_LE_S82 = ['Fracture of patella',###############S82.0 Fracture of patella
                   'Fracture of tibia and fibula',######S82.1 Fracture of upper end of tibia
                                                       #S82142A Bicondylar fracture of tibia
                                                       #S82.2 Fracture of shaft of tibia
                                                       #S82.3 Fracture of lower end of tibia
                   
                                                       #S89.0 Physeal fracture of upper end of tibia
                                                       #S89.1 Physeal fracture of lower end of tibia
                                                       #S89.2 Physeal fracture of upper end of fibula
                                                       #S89.3 Physeal fracture of lower end of fibula
                   
                                                       #S82.87 Pilon fracture of tibia
                   
                                                       #S82.4 Fracture of shaft of fibula
                   
                                                       #S82.81 Torus fracture of upper end of fibula
                                                       #S82.82 Torus fracture of lower end of fibula
                                                       #S82.83 Other fracture of upper and lower end of fibula
                                                       #S82.86 Maisonneuve's fracture
                   
                   'Fracture of ankle']#################S82.5 Fracture of medial malleolus
                                                       #S82.6 Fracture of lateral malleolus
                                                       #S82.84 Bimalleolar fracture of lower leg
                                                       #S82.85 Trimalleolar fracture of lower leg
LE_dict.update(dict.fromkeys(['S82.0'],                                              'Fracture of patella'))
LE_dict.update(dict.fromkeys(['S82.1','S82142A','S82.2','S82.3',
                              'S89.0','S89.1','S89.2','S89.3',
                              'S82.87','S82.4','S82.81','S82.82','S82.83','S82.86'], 'Fracture of tibia and fibula'))
LE_dict.update(dict.fromkeys(['S82.5','S82.6','S82.84','S82.85'],                    'Fracture of ankle'))


#S92: Fracture of foot and toe, except ankle
#S89: Other and unspecified injuries of lower leg
sharfman_LE_other = ['Fracture of LE, other']     #S82.88 Other fractures of lower leg - No desc
                                                  #S82.89 Other fractures of lower leg
                                                  #S82.9 Unspecified fracture of lower leg
                                                  #S92: Fracture of foot and toe, except ankle
                                                  #S89.8: Other specified injuries of lower leg
                                                  #S89.9: Unspecified injury of lower leg
LE_dict.update(dict.fromkeys(['S82.88','S82.89','S82.9','S92','S89.8','S89.9'], 'Fracture of LE, other'))

#List of columns to add
sharfman_LE = sharfman_LE_S32 + sharfman_LE_S72 + sharfman_LE_S82 + sharfman_LE_other

#Categories of UE Injury
#the "sharfman_..._..." lists are names of the new columns to be added
#the LE_ and UE_ dictionaries take an icd10 prefix and direct which column to assign true for a given patient

#S42: Fracture of shoulder and upper arm
sharfman_UE_S42 = ['Fracture of clavicle',        #S42.0 Fracture of clavicle
                   'Fracture of scapula',         #S42.1 Fracture of scapula
                   'Fracture of humerus']         #S42.2 Fracture of upper end of humerus
                                                  #S42.3 Fracture of shaft of humerus
                                                  #S42.4 Fracture of lower end of humerus
#S52: Fracture of forearm
sharfman_UE_S52 = ['Fracture of radius and ulna'] #S52 Fracture of forearm
#S62: Fracture at wrist and hand level
sharfman_UE_S62 = ['Fracture of Carpal bones']    #S62 Fracture at wrist and hand level

sharfman_UE_other = ['Fracture of UE, other']        #S42.9 Fracture of shoulder girdle, part unspecified

UE_dict =      dict.fromkeys(['S42.0']                 , 'Fracture of clavicle')
UE_dict.update(dict.fromkeys(['S42.1']                 , 'Fracture of scapula'))
UE_dict.update(dict.fromkeys(['S42.2', 'S42.3','S42.4'], 'Fracture of humerus'))
UE_dict.update(dict.fromkeys(['S52']                   , 'Fracture of radius and ulna'))
UE_dict.update(dict.fromkeys(['S62']                   , 'Fracture of Carpal bones'))
UE_dict.update(dict.fromkeys(['S42.9']                 , 'Fracture of UE, other'))

sharfman_UE = sharfman_UE_S42+sharfman_UE_S52+sharfman_UE_S62+sharfman_UE_other

#True for "fall on same level - other" #not this only accounts for mechanism codes that begin with W (only ones pertaining to falls)
fall_same_level = [
"W00.0",#:  Fall on same level due to ice and snow
"W01",  #:  Fall on same level from slipping, tripping and stumbling
"W03"   #:  Other fall on same level due to collision with another person
         ]

other_falls = [
"W00.1",#:  Fall from stairs and steps due to ice and snow
"W00.2",#:  Other fall from one level to another due to ice and snow
"W00.9",#:  Unspecified fall due to ice and snow
"W04",#:  Fall while being carried or supported by other persons
"W05",#:  Fall from non-moving wheelchair, nonmotorized scooter and motorized mobility scooter
"W06",#:  Fall from bed
"W07",#:  Fall from chair
"W08",#:  Fall from other furniture
"W09",#:  Fall on and from playground equipment
"W10",#:  Fall on and from stairs and steps
"W11",#:  Fall on and from ladder
"W12",#:  Fall on and from scaffolding
"W13",#:  Fall from, out of or through building or structure
"W14",#:  Fall from tree
"W15",#:  Fall from cliff
"W16",#:  Fall, jump or diving into water
"W17",#:  Other fall from one level to another
"W18",#:  Other slipping, tripping and stumbling and falls
"W19"#:  Unspecified fall
]

for_sure_reject = [
"W20",#:  Struck by thrown, projected or falling object
"W21",#:  Striking against or struck by sports equipment
"W22",#:  Striking against or struck by other objects
"W23",#:  Caught, crushed, jammed or pinched in or between objects
"W24",#:  Contact with lifting and transmission devices, not elsewhere classified
"W25",#:  Contact with sharp glass
"W26",#:  Contact with other sharp objects
"W27",#:  Contact with nonpowered hand tool
"W28",#:  Contact with powered lawn mower
"W29",#:  Contact with other powered hand tools and household machinery
"W30",#:  Contact with agricultural machinery
"W31",#:  Contact with other and unspecified machinery
"W32",#:  Accidental handgun discharge and malfunction
"W33",#:  Accidental rifle, shotgun and larger firearm discharge and malfunction
"W34",#:  Accidental discharge and malfunction from other and unspecified firearms and guns
"W36",#:  Explosion and rupture of gas cylinder
"W37",#:  Explosion and rupture of pressurized tire, pipe or hose
"W38",#:  Explosion and rupture of other specified pressurized devices
"W39",#:  Discharge of firework
"W40",#:  Explosion of other materials
"W45",#:  Foreign body or object entering through skin
"W49",#:  Exposure to other inanimate mechanical forces
"W50",#:  Accidental hit, strike, kick, twist, bite or scratch by another person
"W51",#:  Accidental striking against or bumped into by another person
"W52",#:  Crushed, pushed or stepped on by crowd or human stampede
"W54",#:  Contact with dog
"W55",#:  Contact with other mammals
"W56",#:  Contact with nonvenomous marine animal
"W57",#:  Bitten or stung by nonvenomous insect and other nonvenomous arthropods
"W58",#:  Contact with crocodile or alligator
"W59",#:  Contact with other nonvenomous reptiles
"W60",#:  Contact with nonvenomous plant thorns and spines and sharp leaves
"W61",#:  Contact with birds (domestic) (wild)
"W64",#:  Exposure to other animate mechanical forces
"W85",#:  Exposure to electric transmission lines
"W86",#:  Exposure to other specified electric current
"W94",#:  Exposure to high and low air pressure and changes in air pressure
"Y33",
"V03",
"V00",
"V18"]


fall_same_level_dict =      dict.fromkeys(fall_same_level            ,True)
fall_same_level_dict.update(dict.fromkeys(other_falls+for_sure_reject,False))
fall_dict =      dict.fromkeys(fall_same_level+other_falls, True)
fall_dict.update(dict.fromkeys(for_sure_reject            ,False))

mech_cats = ["Mechanism-Fall","MVT Motorcyclist","MVT Occupant","MVT Other","MVT Pedal cyclist","MVT Pedestrian","MVT Unspecified",
            "Machinery","Struck by, against","Pedal cyclist, other","Pedestrian, other","Transport, other","Mechanism-Other"]
mech_dict =      dict.fromkeys([1,2,4,5,6,17,18,19,20,22,23,24,26,27],"Mechanism-Other")
mech_dict.update(dict.fromkeys([3 ],"Mechanism-Fall"))
mech_dict.update(dict.fromkeys([7 ],"Machinery"))
mech_dict.update(dict.fromkeys([8 ],"MVT Occupant"))
mech_dict.update(dict.fromkeys([9 ],"MVT Motorcyclist"))
mech_dict.update(dict.fromkeys([10],"MVT Pedal cyclist"))
mech_dict.update(dict.fromkeys([11],"MVT Pedestrian"))
mech_dict.update(dict.fromkeys([12],"MVT Unspecified"))
mech_dict.update(dict.fromkeys([13],"MVT Other"))
mech_dict.update(dict.fromkeys([14],"Pedal cyclist, other"))
mech_dict.update(dict.fromkeys([15],"Pedestrian, other"))
mech_dict.update(dict.fromkeys([16],"Transport, other"))
mech_dict.update(dict.fromkeys([21],"Struck by, against"))


sex_dict =      dict.fromkeys(["Male"  ,"1.0",1],"Sex-Male")
sex_dict.update(dict.fromkeys(["Female","2.0",2],"Sex-Female"))
sex_dict.update(dict.fromkeys([               3],"Sex-Nonbinary"))
race_dict =      dict.fromkeys(['White, Hispanic','White, non-Hispanic']             ,"Race-White")
race_dict.update(dict.fromkeys(['Black or African American']                         ,"Race-Black"))
race_dict.update(dict.fromkeys(['Other','Pacific Islander','American Indian','Asian'],"Race-Other"))


df = pd.read_csv('geriatric_processed_data_17_23.csv')
N = len(df)

#Insert new columns
for i in range(len(sharfman_LE)): #sharfman labels for LE fractures
    df.insert(4+i,sharfman_LE[i],False)

for i in range(len(sharfman_UE)): #sharfman labels for UE fractures
    df.insert(4+len(sharfman_LE)+i,sharfman_UE[i],False)

fracture_groups = ["ILE","MLE","ULE"] #this is to add new T/F columns for ILE, MLE, ULE
for i in range(len(fracture_groups)):
    df.insert(df.columns.get_loc("Fracture Type")+1+i,fracture_groups[i],False)

#df.insert(df.columns.get_loc("Race Complete")+1,"Race Filtered","Other")
races = ["Race-White","Race-Black","Race-Other","Hispanic"]
for i in range(len(races)):
    df.insert(df.columns.get_loc("Race Complete")+1+i,races[i],False)

sexes = ["Sex-Male","Sex-Female","Sex-Nonbinary"]
for i in range(len(sexes)):
    df.insert(df.columns.get_loc("SEX")+1+i,sexes[i],False)

df.insert(df.columns.get_loc("primaryecodeicd10")+1,"fall on same level - other",False)
df.insert(df.columns.get_loc("primaryecodeicd10")+2,"any fall",False)

for i in range(len(mech_cats)):
    df.insert(df.columns.get_loc("mechanism")+1+i,mech_cats[i],False)
    
payment_methods = ['Payment-Medicare','Payment-Private/Commercial Insurance','Payment-Medicaid','Payment-Other Government','Payment-Self-Pay','Payment-Other','Payment-Not Billed (for any reason)']
for i in range(len(payment_methods)):
    df.insert(df.columns.get_loc("primarymethodpayment")+1+i,payment_methods[i],False)

df.insert(df.columns.get_loc("hospdischargedisposition")+1,"discharge_to_rehab",False)

df.insert(df.columns.get_loc("totaliculos")+1,"long_ICU",False)

df.insert(df.columns.get_loc("Ventilator-Associated Pneumonia")+1,"Any Complications",False)


df.insert(df.columns.get_loc("Pregnancy")+1,                                                "Pregnancy T-F",False)
df.insert(df.columns.get_loc("Bipolar I/II Disorder")+1,                        "Bipolar I/II Disorder T-F",False)
df.insert(df.columns.get_loc("Major Depressive Disorder")+1,                "Major Depressive Disorder T-F",False)
df.insert(df.columns.get_loc("Other Mental/Personality Disorder")+1,"Other Mental/Personality Disorder T-F",False)
df.insert(df.columns.get_loc("Post-Traumatic Stress Disorder")+1,      "Post-Traumatic Stress Disorder T-F",False)
df.insert(df.columns.get_loc("Schizoaffective Disorder")+1,                  "Schizoaffective Disorder T-F",False)
df.insert(df.columns.get_loc("Schizophrenia")+1,                                        "Schizophrenia T-F",False)

comorbities = ['Attention Deficit Hyperactivity Disorder', 'Advanced Directive Limiting Care', 
               'Alcohol Use Disorder', 'Angina Pectoris', 'Anticoagulant Therapy', 'Bleeding Disorder', 
               'Currently Receiving Chemotherapy for Cancer', 'Cirrhosis', 'Congenital Anomalies', 
               'Chronic Obstructive Pulmonary Disease', 'Cerebrovascular Accident', 'Dementia', 'Diabetes Mellitus', 
               'Disseminated Cancer', 'Functionaly Dependent Health Status', 'Congestive Heart Failure', 'Hypertension', 
               'Myocardial Infarction_pc', 'Other_pc', 'Peripheral Arterial Disease', 'Prematurity', 'Mental/Personality Disorder', 
               'Chronic Renal Failure', 'Current Smoker', 'Steroid Use', 'Substance Abuse Disorder']

for i in range(len(comorbities)):
    df.insert(df.columns.get_loc(comorbities[i])+1,comorbities[i]+" T-F",False)
    
for i in range(len(list(df["GCS"].unique()))):
    df.insert(df.columns.get_loc("GCS")+1+i,list(df["GCS"].unique())[i],False)

for i in range(len(list(df["ISS"].unique()))):
    df.insert(df.columns.get_loc("ISS")+1+i,list(df["ISS"].unique())[i],False)

print(df.columns.tolist()) #confirm the new columns were added in the order you like

#Removal Criteria

remove = []
for index, patient in df.iterrows():

    #Race - Unknown
    if patient["Race Complete"] == 'Unknown':
        #remove.append(index)
        #dont actually remove!
        df.loc[index,"Race Complete"] = np.nan
        #print("Remove patient ",index," - basis: race ",patient["Race Complete"])

    #Primary Method Payment - NaN
    if str(patient['primarymethodpayment']) == 'nan':
        #remove.append(index)
        #dont actually remove!
        df.loc[index,'primarymethodpayment'] = np.nan
        #print("Remove patient ",index," - basis: payment ",patient["primarymethodpayment"])
    
    #Sex - NaN
    if str(patient['SEX']) == 'nan':
        #remove.append(index)
        #dont actually remove!
        df.loc[index,'SEX'] = np.nan
        #print("Remove patient ",index," - basis: SEX ",patient["SEX"])
    #eddischargedisposition - NaN
    if str(patient['eddischargedisposition']) == 'nan':
        #remove.append(index)
        #dont actually remove!
        df.loc[index,'eddischargedisposition'] = np.nan
        #print("Remove patient ",index," - basis: eddischargedisposition ",patient["eddischargedisposition"])
    #hospdischargedisposition - NaN
    if str(patient['hospdischargedisposition']) == 'nan':
        #remove.append(index)
        #dont actually remove!
        df.loc[index,'hospdischargedisposition'] = np.nan
        #print("Remove patient ",index," - basis: hospdischargedisposition ",patient["hospdischargedisposition"])
    if str(patient['mechanism'])=='nan':
        #remove.append(index)
        #dont actually remove!
        df.loc[index,'mechanism'] = np.nan
        #print("Remove patient ",index," - basis: mechanism ",patient["mechanism"])
    #Spinal Injury
    LE_codes = csc(patient['LE_Dcode'])
    for i in range(len(LE_codes)):
        code = LE_codes[i]
        if ("S32.0" in code or "S32.1" in code or "S32.2" in code): #ignore spine (not included in sharfman labels)
            if index not in remove: 
                remove.append(index)
                print("Remove patient ",index," - basis: spine injury ",patient["LE_Dcode"])
    
df = df.drop(index=remove)
N2 = len(df)
print("Dropped ",len(remove)," patients. From ",N," to ",N2)

#New Columns: Race, , Sex, Payment, days ICU, discharge to rehab

for index, patient in df.iterrows():
    
    race = patient["Race Complete"]
    if str(race) != 'nan':
        df.loc[index,race_dict[race]] = True
    if race == 'White, Hispanic':
        df.loc[index,"Hispanic"] = True

    sex = patient["SEX"]
    if str(sex) != 'nan':
        df.loc[index,sex_dict[sex]] = True

    payment = patient['primarymethodpayment']
    if payment == "Private/Commerical Insurance":
        df.loc[index,"Payment-Private/Commercial Insurance"] = True
    elif str(payment) != 'nan':
        df.loc[index,"Payment-"+payment] = True
        

    discharge = patient['hospdischargedisposition']
    if (discharge == "Discharged/Transferred to inpatient rehab or designated unit") or (discharge == '11.0') or (discharge == 11): #or ('11' in discharge)
        df.loc[index,"discharge_to_rehab"] = True
        
    icu_length = patient['totaliculos']
    if icu_length == '>3d':
        df.loc[index,"long_ICU"] = True
    elif icu_length == '<=3d':
        df.loc[index,"long_ICU"] = False

    #Any complications from deep ssi to ventilator associated pneumonia
    df.loc[index,"Any Complications"]= anycomp(list(df.loc[index,"Central Line-associated Bloodstream Infection":"Ventilator-Associated Pneumonia"]))

    #Data Cleanup for Pregnancy to Schizophrenia
    query0 = ['Pregnancy T-F','Bipolar I/II Disorder T-F','Major Depressive Disorder T-F','Other Mental/Personality Disorder T-F',
             'Post-Traumatic Stress Disorder T-F','Schizoaffective Disorder T-F','Schizophrenia T-F']
    for i in range(len(query0)):
        if str(patient[query0[i][:-4]])=='nan':
            df.loc[index,query0[i]] = False
        else:
            df.loc[index,query0[i]] = True

    df.loc[index,patient["GCS"]] = True
    df.loc[index,patient["ISS"]] = True
    #progressbar(index+1,N2)


#ICD10 codes to T/F diagnoses, mechanisms
for index, patient in df.iterrows():

    #Pull codes
    LE_codes = csc(patient['LE_Dcode'])
    if isinstance(patient['UE_Dcode'],str):
        UE_codes = csc(patient['UE_Dcode'])
    else:
        UE_codes = []

    #ILE,MLE,ULE Types
    if   len(LE_codes) >= 1 and len(UE_codes) >= 1:
        fracture_type  = "ULE"
        df.loc[index,"ULE"] = True
    elif len(LE_codes) >  1 and len(UE_codes) == 0:
        fracture_type  = "MLE"
        df.loc[index,"MLE"] = True
    elif len(LE_codes) == 1 and len(UE_codes) == 0:
        fracture_type  = "ILE"
        df.loc[index,"ILE"] = True
        #if patient['Fracture Type'] != "ILE": #so far no disagreements!
        #    print("ILE disaggreement for patient ",index,", LE codes: ",patient['LE_Dcode'],", UE codes: ",patient['UE_Dcode'],", fracture type: ",patient['Fracture Type'])
        #    break
    #else: #so far not an issue!
    #    print("couldn't assign fracture type to patient ",index,", LE codes: ",patient['LE_Dcode'],", UE codes: ",patient['UE_Dcode'])
    #    break

    #Decode LE ICD10s and assign trues to appropriate columns
    for i in range(len(LE_codes)):
        code = LE_codes[i]
        
        try:
            search = code #S82.231A
            diagnosis = LE_dict[search]
        except:
            try: 
                search = code[:7] #S82.231
                diagnosis = LE_dict[search]
            except: 
                try:
                    search = code[:6] #S82.23
                    diagnosis = LE_dict[search]
                except:
                    try: 
                        search = code[:5] #S82.2
                        diagnosis = LE_dict[search]
                    except:
                        try:
                            search = code[:3] #S82
                            diagnosis = LE_dict[search]
                        except:
                            print("Cant match a diagnosis to this code! - LE ",code)
        df.loc[index,diagnosis] = True

    #Decode UE ICD10s and assign trues to appropriate columns
    for i in range(len(UE_codes)):
        code = UE_codes[i]
        try: 
            search = code[:5]
            diagnosis = UE_dict[search]
        except:
            try:
                search = code[:3]
                diagnosis = UE_dict[search]
            except:
                print("Cant match a diagnosis to this code! - UE ",code)
        df.loc[index,diagnosis] = True

    #Decode mechanism code for fall on same level other?
    w_code = patient['primaryecodeicd10']
    try: 
        search = w_code[:5]
        df.loc[index,"fall on same level - other"] = fall_same_level_dict[search]
    except:
        try:
            search = w_code[:3]
            df.loc[index,"fall on same level - other"] = fall_same_level_dict[search]
        except:
            print("Cant match a mechanism code to this code! - ",w_code)
            
    #mechanism - fall or?
    mechanism_no = patient['mechanism']
    if str(mechanism_no) != 'nan':
        mechanism = mech_dict[mechanism_no]
        print("Patient ",index," mechanism: ",mechanism_no," ",mechanism)
        df.loc[index,mechanism] = True
    
    #comorbidities
    for j in range(len(comorbities)):
        value = df.loc[index,comorbities[j]]
        try:
            if float(value) >= 1:
                df.loc[index,comorbities[j]+" T-F"] = True
            #elif float(value) == 0:
            #    df.loc[index,comorbities[j]+" T-F"] = False
        except:
            if value == "True":
                df.loc[index,comorbities[j]+" T-F"] = True
            #elif value == "False" or str(value)=='nan':
            #    df.loc[index,comorbities[j]+" T-F"] = False


#Generate Table 1
factors1 = ["Sex-Female"]+races+comorbities+payment_methods
table1 = pd.DataFrame(columns = fracture_groups+['p-value'],
                     index=['N']+factors1)

Ns = [len(df.loc[df["ILE"] == True]),
      len(df.loc[df["MLE"] == True]),
      len(df.loc[df["ULE"] == True])]
table1.loc['N'] = Ns+[None] #check sum(Ns)

table1.loc["Sex-Female"] = calculate(df,"Sex-Female",Ns,True)
for i in range(len(races)):
    table1.loc[races[i]] = calculate(df,races[i],Ns,True)
for i in range(len(comorbities)):
    table1.loc[comorbities[i]] = calculate(df,comorbities[i]+" T-F",Ns,True)
for i in range(len(payment_methods)):
    table1.loc[payment_methods[i]] = calculate(df,payment_methods[i],Ns,True)

table1

factors2 = mech_cats+sharfman_LE+sharfman_UE+list(df["GCS"].unique())+list(df["ISS"].unique())
table2 = pd.DataFrame(columns = fracture_groups+['p-value'],
                     index=['N']+factors2)

Ns = [len(df.loc[df["ILE"] == True]),
      len(df.loc[df["MLE"] == True]),
      len(df.loc[df["ULE"] == True])]
table2.loc['N'] = Ns+[None] #check sum(Ns)


for i in range(len(factors2)):
    if sum(df[factors2[i]])==0:
        continue
    table2.loc[factors2[i]] = calculate(df,factors2[i],Ns,True)

table2

df.to_csv("cleaned_data_VR.csv", index=False)
table1.to_csv("cleaned_table1_VR.csv", index=True)
table2.to_csv("cleaned_table2_VR.csv", index=True)