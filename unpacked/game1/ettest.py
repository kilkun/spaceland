#################################
###------- FILE TO LOAD ------###
#################################
#THIS IS THE ONLY THING YOU SHOULD CHANGE#
FILE_TO_LOAD = 'map01.xml'


##################################
###------ INITIALIZATION ------###
##################################
import xml.etree.ElementTree as et
#get our initial vars set up
print("Loading ",FILE_TO_LOAD)
tree = et.parse(FILE_TO_LOAD)
item_index = et.parse('wasteland_item_ids.xml')
print("Parsing...")
root = tree.getroot()
items = item_index.getroot()
print("Done.")
#map size variable
print("Reading map size...")
mapsize = int(root.get('mapSize'))
print("Mapsize set as: ",mapsize," tiles.")


#############################
##------- MAPS SHIT -------##
#############################
#Our first two map classes is born are a variable
#Don't need to worry about these because its always the first in the file
print("Loading Action Class Map String...")
acmap = root[0].text
print("Done.")
print("Loading Action Map String...")
amap = root[1].text
print("Done.")
#the tilemap isn't hardcoded because it's position changes
print("Loading Tile Map String...")
tmap = root.find('tileMap').text
print("Done.")
print("Cleaning Strings...")
acmap = acmap.replace("\n","*") #Allows us to easily pick apart the strings
amap = amap.replace("\n","")
tmap = tmap.replace("\n","")
acmap = acmap.replace(" ","")
amap = amap.replace(" ","")
tmap = tmap.replace(" ","")
print("Done.")

#initialize our map arrays
acmap_array = [[0 for j in range(mapsize)]for j in range(mapsize)] 
amap_array = [[0 for j in range(mapsize)]for j in range(mapsize)]
tmap_array = [[0 for j in range(mapsize)]for j in range(mapsize)]

#Store the action class map in an array
column = 0 
row = 0
print("Initializing Action Class Map...")
for i in acmap:
    if(i!="*"): #if the character != * then store it in the array
        acmap_array[row][column] = i
        column+=1
        if(column==mapsize):
            column=0
            if(row!=mapsize-1):
                row+=1
print("ACM Initialized.")

###NOTE TO SELF: This can probably be genericized into a function. Should do that for cleaner code.###
#Store the action map in an array
column = 0
row = 0
count = 0
n=2
print("Initializing Action Map...")
temp_amap_array = [amap[i:i+n] for i in range(0, len(amap), n)]
for i in temp_amap_array:
    amap_array[row][column] = i
    column+=1
    if(column==mapsize):
        column = 0
        if(row!=mapsize-1):
            row+=1
print("AM Initiliazed")
#Store the tile map in an array
column = 0
row = 0
count = 0
n=2
print("Initializing Tile Map...")
temp_tmap_array = [tmap[i:i+n] for i in range(0, len(tmap), n)]
for i in temp_tmap_array:
    tmap_array[row][column] = i
    column+=1
    if(column==mapsize):
        column = 0
        if(row!=mapsize-1):
            row+=1
print("TM Initiliazed")

################################
##------- STRINGS SHIT -------##
################################
print("Loading Strings...")
maxstringnum = 0
for i in root.find('strings'):
    num = int(i.get('id'))
    if( num > maxstringnum):
        maxstringnum = num
string_array = [0 for i in range(maxstringnum+1)]#sloppy but it werks
for i in root.find('strings'):
    ind = int(i.get('id'))
    string_array[ind] = i.text
print("Strings indexed.")

##################################
###------ ACTION CLASSES ------###
##################################

print("Loading block message data...")
array_block = []
for i in root.find('actions'):
    array_block.append(int(i.get('message')))#This is clean, it stores the string information and lines up the id with the index!
print("Block class data loaded.")

print("Loading loot bag data...")
array_loot = []
#load stuff from <actions actionClass="0x5">
print("Loot bag data loaded.")

print("Loading transition data...")
array_trans = []
#load stuff from <actions actionClass="0xa">
print("Transition data loaded.")

##############################
###------- MAP INFO -------###
##############################
#holds stuff like the tileset, the background tile, the scale (scale probably isn't gonna change), encounter frequency, etc.
#stuff to change via info boxes


####################################
###------- BATTLE STRINGS -------###
####################################
#what the game will say when shit is going down in battle, ie blasting people into thin red pastes!


### That should do it for basic data loading ###
### More stuff exists but that can be added once we have everything here loaded up. ###
