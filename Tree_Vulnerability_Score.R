###########################################################################################################################################################
# Title: North American CAPTURE Tree species vulnerability using Kevin Potter's A United States national prioritization framework for tree species 
#        vulnerability to climate change
# Writer: Soren Donisvitch 
# Date 4/20/22
# Cited material: Potter, Kevin M.; Crane, Barbara S.; Hargrove, William W. 2017.A United States national prioritization framework for 
#                 tree species vulnerability to climate change. New Forests. 48: 275-300. 26 p.  
#                 https://doi.org/10.1007/s11056-017-9569-5.
#
# Contact: soren.donisvitch@gmail.com
# Requirements: Requires tree species names to be formatted as scientific name (Genus species) with capitalized genus and lowercase species
# Complementary functions: FIA_species_translation (translates FIA and scientific names of common North American Tree species)
###########################################################################################################################################################

# Tree species vulnerability to climate change (VCC) function produces primary scores and auxiliary info from Potter, 2017,. CAPTURE framework
# Inputs: 
# species = "Genus species" : character variable input of tree species scientific name  
# desired_return = "score" : returns tree score in regard to climate vulnerability (higher = more vulnerable: lower = less vulnerable)
#                = "all" : returns list() of all variables from Potter, 2017,. 
#                          - name  = common name (tree species) [2]
#                          - CE = Climate Exposure [3]
#                          - S = Sensitivity [4]
#                          - LAC = Low Adaptive Capacity [5]
#                          - cluster = primary clustering in framework analysis [6]
#                                      -- E2 =  Low current vulnerability	
#                                      -- E4 = Low current vulnerability	
#                                      -- A  = High vulnerability, low persistence/adaptation	
#                                      -- D  = Potential high future vulnerability	
#                                      -- B  = High vulnerability, potential adaptation
#                                      -- E1 = Low current vulnerability	
#                                      -- C  = High vulnerability, potential persistence
#                          -c_rank = inter-cluster ranking (high -> low) [7]
# 

VCC = function(species,desired_return){
       if(species == 'Abies amabilis'){ name = 'Pacific silver fir';CE = 8.25;S = 57.42;LAC = 49.75;score =38.48; cluster = 'E2'; c_rank =7}
  else if(species == 'Abies balsamea'){ name = 'balsam fir ';CE = 16.5;S = 48.36;LAC = 43.56;score =36.14; cluster = 'E2'; c_rank =13}
  else if(species == 'Abies bracteata'){ name = 'Bristlecone fir';CE = 58;S = 86.8;LAC = 63.19;score =69.33; cluster = 'A'; c_rank =16}
  else if(species == 'Abies concolor'){ name = 'white fir';CE = 8.25;S = 43.75;LAC = 43.93;score =31.98; cluster = 'E2'; c_rank =27}
  else if(species == 'Abies fraseri'){ name = 'Fraser fir ';CE = 33.25;S = 69.24;LAC = 56.97;score =53.15; cluster = 'D'; c_rank =11}
  else if(species == 'Abies grandis'){ name = 'grand fir';CE = 0;S = 41.82;LAC = 48.85;score =30.23; cluster = 'E2'; c_rank =30}
  else if(species == 'Abies lasiocarpa'){ name = 'subalpine fir';CE = 0;S = 48.36;LAC = 41.56;score =29.97; cluster = 'E2'; c_rank =32}
  else if(species == 'Abies magnifica'){ name = 'California red fir';CE = 33;S = 56.61;LAC = 56.11;score =48.57; cluster = 'D'; c_rank =27}
  else if(species == 'Abies procera'){ name = 'noble fir';CE = 0;S = 54.34;LAC = 44.22;score =32.85; cluster = 'E2'; c_rank =23}
  else if(species == 'Acacia farnesiana'){ name = 'sweet acacia';CE = 57.75;S = 28.88;LAC = 53.54;score =46.72; cluster = 'C'; c_rank =58}
  else if(species == 'Acer barbatum'){ name = 'Florida maple';CE = 83;S = 57.42;LAC = 64.48;score =68.3; cluster = 'A'; c_rank =19}
  else if(species == 'Acer glabrum'){ name = 'Rocky Mountain maple';CE = 24.75;S = 47.8;LAC = 48.84;score =40.47; cluster = 'D'; c_rank =48}
  else if(species == 'Acer grandidentatum'){ name = 'bigtooth maple';CE = 41.25;S = 55.02;LAC = 46.11;score =47.46; cluster = 'B'; c_rank =37}
  else if(species == 'Acer leucoderme'){ name = 'Chalk maple';CE = 100;S = 65.15;LAC = 69.09;score =78.08; cluster = 'A'; c_rank =3}
  else if(species == 'Acer macrophyllum'){ name = 'bigleaf maple';CE = 0;S = 46.7;LAC = 56.6;score =34.43; cluster = 'E2'; c_rank =19}
  else if(species == 'Acer negundo'){ name = 'boxelder ';CE = 74.75;S = 36.15;LAC = 39.31;score =50.07; cluster = 'E1'; c_rank =13}
  else if(species == 'Acer nigrum'){ name = 'black maple ';CE = 91.5;S = 53.72;LAC = 51.4;score =65.54; cluster = 'C'; c_rank =5}
  else if(species == 'Acer pensylvanicum'){ name = 'striped maple ';CE = 41.25;S = 49.04;LAC = 46.42;score =45.57; cluster = 'B'; c_rank =38}
  else if(species == 'Acer saccharinum'){ name = 'silver maple ';CE = 91.5;S = 46.09;LAC = 44.83;score =60.81; cluster = 'C'; c_rank =15}
  else if(species == 'Acer spicatum'){ name = 'mountain maple ';CE = 49.5;S = 40.72;LAC = 39.03;score =43.08; cluster = 'C'; c_rank =67}
  else if(species == 'Acoelorrhaphe wrightii'){ name = 'Everglades palm';CE = 57.75;S = 64.27;LAC = 72.3;score =64.77; cluster = 'A'; c_rank =26}
  else if(species == 'Aesculus californica'){ name = 'California buckeye';CE = 8.25;S = 64.27;LAC = 45.25;score =39.26; cluster = 'E2'; c_rank =6}
  else if(species == 'Aesculus flava'){ name = 'Yellow buckeye';CE = 74.75;S = 65.69;LAC = 62.41;score =67.61; cluster = 'A'; c_rank =21}
  else if(species == 'Aesculus glabra'){ name = 'Ohio buckeye';CE = 91.5;S = 58.54;LAC = 58.63;score =69.56; cluster = 'A'; c_rank =15}
  else if(species == 'Aesculus pavia'){ name = 'red buckeye';CE = 91.5;S = 82.44;LAC = 49.34;score =74.42; cluster = 'B'; c_rank =1}
  else if(species == 'Alnus maritima'){ name = 'seaside alder';CE = 74.75;S = 68.47;LAC = 41.75;score =61.66; cluster = 'B'; c_rank =13}
  else if(species == 'Alnus oblongifolia'){ name = 'Arizona alder';CE = 8.25;S = 80.27;LAC = 36.94;score =41.82; cluster = 'E2'; c_rank =3}
  else if(species == 'Alnus rhombifolia'){ name = 'white alder';CE = 41.25;S = 57.19;LAC = 29.5;score =42.64; cluster = 'B'; c_rank =41}
  else if(species == 'Amelanchier arborea'){ name = 'common serviceberry ';CE = 74.5;S = 28.99;LAC = 29.63;score =44.38; cluster = 'E1'; c_rank =23}
  else if(species == 'Amelanchier sanguinea'){ name = 'roundleaf serviceberry ';CE = 74.5;S = 42.52;LAC = 33.3;score =50.11; cluster = 'E1'; c_rank =12}
  else if(species == 'Amyris elemifera'){ name = 'sea torchwood';CE = 16.5;S = 62.69;LAC = 50.62;score =43.27; cluster = 'D'; c_rank =44}
  else if(species == 'Annona glabra'){ name = 'pond-apple';CE = 49.75;S = 58.42;LAC = 42.24;score =50.14; cluster = 'B'; c_rank =30}
  else if(species == 'Arbutus arizonica'){ name = 'Arizona madrone';CE = 33;S = 53.39;LAC = 52.21;score =46.2; cluster = 'D'; c_rank =37}
  else if(species == 'Arbutus menziesii'){ name = 'Pacific madrone';CE = 0;S = 31.02;LAC = 57.32;score =29.44; cluster = 'E2'; c_rank =33}
  else if(species == 'Arbutus xalapensis'){ name = 'Texas madrone';CE = 41.25;S = 71.92;LAC = 56.97;score =56.71; cluster = 'D'; c_rank =3}
  else if(species == 'Asimina triloba'){ name = 'pawpaw ';CE = 100;S = 33.12;LAC = 67.04;score =66.72; cluster = 'C'; c_rank =3}
  else if(species == 'Avicennia germinans'){ name = 'black-mangrove';CE = 41.5;S = 57.35;LAC = 53.16;score =50.67; cluster = 'D'; c_rank =22}
  else if(species == 'Betula alleghaniensis'){ name = 'yellow birch ';CE = 16.5;S = 43.07;LAC = 32.79;score =30.79; cluster = 'E2'; c_rank =29}
  else if(species == 'Betula nigra'){ name = 'river birch ';CE = 83;S = 36.46;LAC = 33.55;score =51; cluster = 'E1'; c_rank =11}
  else if(species == 'Betula occidentalis'){ name = 'water birch';CE = 41.25;S = 51.52;LAC = 34.5;score =42.42; cluster = 'B'; c_rank =42}
  else if(species == 'Betula populifolia'){ name = 'gray birch ';CE = 0;S = 43.75;LAC = 25.6;score =23.12; cluster = 'E2'; c_rank =44}
  else if(species == 'Bursera simaruba'){ name = 'gumbo limbo';CE = 33;S = 62.69;LAC = 47.3;score =47.66; cluster = 'B'; c_rank =36}
  else if(species == 'Calocedrus decurrens'){ name = 'incense-cedar';CE = 0;S = 47.2;LAC = 35.29;score =27.5; cluster = 'E2'; c_rank =36}
  else if(species == 'Carnegiea gigantea'){ name = 'saguaro';CE = 16.5;S = 69.4;LAC = 56.38;score =47.43; cluster = 'D'; c_rank =31}
  else if(species == 'Carya aquatica'){ name = 'water hickory ';CE = 74.5;S = 36.08;LAC = 47.44;score =52.67; cluster = 'C'; c_rank =42}
  else if(species == 'Carya cordiformis'){ name = 'bitternut hickory ';CE = 66.25;S = 37.45;LAC = 41.77;score =48.49; cluster = 'C'; c_rank =52}
  else if(species == 'Carya illinoinensis'){ name = 'pecan';CE = 91.5;S = 36.35;LAC = 44.39;score =57.41; cluster = 'C'; c_rank =23}
  else if(species == 'Carya laciniosa'){ name = 'shellbark hickory ';CE = 100;S = 41.37;LAC = 50.27;score =63.88; cluster = 'C'; c_rank =8}
  else if(species == 'Carya myristiciformis'){ name = 'Nutmeg hickory';CE = 74.75;S = 61.48;LAC = 57.98;score =64.74; cluster = 'A'; c_rank =27}
  else if(species == 'Carya ovata'){ name = 'shagbark hickory ';CE = 49.5;S = 44.58;LAC = 37.87;score =43.98; cluster = 'C'; c_rank =65}
  else if(species == 'Carya pallida'){ name = 'sand hickory ';CE = 100;S = 34.02;LAC = 53.5;score =62.51; cluster = 'C'; c_rank =10}
  else if(species == 'Carya texana'){ name = 'black hickory';CE = 66.25;S = 41.58;LAC = 35.51;score =47.78; cluster = 'C'; c_rank =57}
  else if(species == 'Castanea dentata'){ name = 'American chestnut';CE = 49.5;S = 67.52;LAC = 77.61;score =64.88; cluster = 'A'; c_rank =25}
  else if(species == 'Castanea pumila'){ name = 'Allegheny chinquapin';CE = 83.25;S = 60.19;LAC = 66.84;score =70.09; cluster = 'A'; c_rank =12}
  else if(species == 'Castanea pumila var. ozarkensis'){ name = 'Ozark chinquapin';CE = 83.25;S = 68.44;LAC = 73.95;score =75.21; cluster = 'A'; c_rank =7}
  else if(species == 'Catalpa bignonioides'){ name = 'southern catalpa';CE = 83.25;S = 68.76;LAC = 35.17;score =62.39; cluster = 'B'; c_rank =12}
  else if(species == 'Catalpa speciosa'){ name = 'northern catalpa';CE = 100;S = 58.32;LAC = 37.86;score =65.39; cluster = 'B'; c_rank =10}
  else if(species == 'Celtis laevigata'){ name = 'sugarberry ';CE = 74.5;S = 28.99;LAC = 24.1;score =42.53; cluster = 'E1'; c_rank =25}
  else if(species == 'Celtis occidentalis'){ name = 'hackberry ';CE = 83;S = 24.35;LAC = 27.77;score =45.04; cluster = 'E1'; c_rank =22}
  else if(species == 'Cercis canadensis'){ name = 'eastern redbud ';CE = 83;S = 20.91;LAC = 38.14;score =47.35; cluster = 'E1'; c_rank =17}
  else if(species == 'Chamaecyparis lawsoniana'){ name = 'Port-Orford-cedar';CE = 16.5;S = 53.99;LAC = 42.55;score =37.68; cluster = 'E2'; c_rank =9}
  else if(species == 'Chamaecyparis nootkatensis'){ name = 'Alaska yellow-cedar';CE = 0;S = 56.61;LAC = 41.75;score =32.79; cluster = 'E2'; c_rank =24}
  else if(species == 'Chamaecyparis thyoides'){ name = 'Atlantic whitecedar';CE = 33.25;S = 59.52;LAC = 47.29;score =46.69; cluster = 'D'; c_rank =36}
  else if(species == 'Chrysolepis chrysophylla var. chrysophylla'){ name = 'golden chinquapin';CE = 8.25;S = 63.27;LAC = 30.28;score =33.93; cluster = 'E2'; c_rank =21}
  else if(species == 'Cladrastis kentukea'){ name = 'Yellowwood';CE = 83.25;S = 61.21;LAC = 51.26;score =65.24; cluster = 'A'; c_rank =24}
  else if(species == 'Coccoloba diversifolia'){ name = 'tietongue';CE = 24.75;S = 65.46;LAC = 54.67;score =48.29; cluster = 'D'; c_rank =30}
  else if(species == 'Coccothrinax argentata'){ name = 'Florida silver palm';CE = 0;S = 80.03;LAC = 35.17;score =38.4; cluster = 'E2'; c_rank =8}
  else if(species == 'Colubrina elliptica'){ name = 'soldierwood';CE = 33;S = 60.39;LAC = 59.26;score =50.88; cluster = 'D'; c_rank =21}
  else if(species == 'Condalia hookeri'){ name = 'bluewood';CE = 83.25;S = 45.7;LAC = 41.41;score =56.79; cluster = 'C'; c_rank =26}
  else if(species == 'Conocarpus erectus'){ name = 'buttonwood-mangrove';CE = 41.5;S = 41.63;LAC = 53.47;score =45.53; cluster = 'D'; c_rank =39}
  else if(species == 'Cordia sebestena'){ name = 'Longleaf geiger tree';CE = 49.75;S = 80.03;LAC = 60.36;score =63.38; cluster = 'A'; c_rank =29}
  else if(species == 'Cornus nuttallii'){ name = 'Pacific dogwood';CE = 0;S = 53.74;LAC = 48.06;score =33.94; cluster = 'E2'; c_rank =20}
  else if(species == 'Cotinus obovatus'){ name = 'smoketree ';CE = 91.5;S = 45.7;LAC = 58.95;score =65.38; cluster = 'C'; c_rank =6}
  else if(species == 'Crataegus brainerdii'){ name = 'Brainerds hawthorn';CE = 100;S = 51.89;LAC = 32.16;score =61.35; cluster = 'C'; c_rank =13}
  else if(species == 'Crataegus calpodendron'){ name = 'pear hawthorn';CE = 100;S = 29.2;LAC = 30.4;score =53.2; cluster = 'E1'; c_rank =8}
  else if(species == 'Crataegus chrysocarpa'){ name = 'fireberry hawthorn';CE = 100;S = 29.2;LAC = 35.94;score =55.05; cluster = 'E1'; c_rank =6}
  else if(species == 'Crataegus crus-galli'){ name = 'cockspur hawthorn';CE = 100;S = 24.95;LAC = 19.08;score =48.01; cluster = 'E1'; c_rank =16}
  else if(species == 'Crataegus dilatata'){ name = 'broadleaf hawthorn';CE = 100;S = 43.64;LAC = 26.42;score =56.69; cluster = 'E1'; c_rank =1}
  else if(species == 'Crataegus flabellata'){ name = 'fanleaf hawthorn';CE = 100;S = 29.2;LAC = 20.99;score =50.06; cluster = 'E1'; c_rank =14}
  else if(species == 'Crataegus mollis'){ name = 'downy hawthorn';CE = 100;S = 24.95;LAC = 13.01;score =45.99; cluster = 'E1'; c_rank =21}
  else if(species == 'Crataegus pedicellata'){ name = 'scarlet hawthorn';CE = 100;S = 43.64;LAC = 26.42;score =56.69; cluster = 'E1'; c_rank =2}
  else if(species == 'Cupressus abramsiana'){ name = 'Santa Cruz cypress';CE = 66.5;S = 70.26;LAC = 62.22;score =66.33; cluster = 'A'; c_rank =23}
  else if(species == 'Cupressus arizonica'){ name = 'Arizona cypress';CE = 41.25;S = 53.5;LAC = 56.08;score =50.28; cluster = 'D'; c_rank =23}
  else if(species == 'Cupressus bakeri'){ name = 'Baker cypress';CE = 24.75;S = 66.95;LAC = 64.63;score =52.11; cluster = 'D'; c_rank =16}
  else if(species == 'Cupressus forbesii'){ name = 'Tecate cypress';CE = 33;S = 70.26;LAC = 62.22;score =55.16; cluster = 'D'; c_rank =7}
  else if(species == 'Cupressus goveniana'){ name = 'Gowen cypress';CE = 0;S = 78.34;LAC = 62.22;score =46.85; cluster = 'D'; c_rank =35}
  else if(species == 'Cupressus macnabiana'){ name = 'Macnabs cypress';CE = 24.75;S = 66;LAC = 65.96;score =52.24; cluster = 'D'; c_rank =14}
  else if(species == 'Cupressus macrocarpa'){ name = 'Monterey cypress';CE = 57.75;S = 58.57;LAC = 59.28;score =58.53; cluster = 'D'; c_rank =1}
  else if(species == 'Cupressus sargentii'){ name = 'Sargents cypress';CE = 0;S = 53.42;LAC = 58.44;score =37.29; cluster = 'E2'; c_rank =10}
  else if(species == 'Diospyros texana'){ name = 'Texas persimmon';CE = 83;S = 41.54;LAC = 44.18;score =56.24; cluster = 'C'; c_rank =29}
  else if(species == 'Diospyros virginiana'){ name = 'common persimmon ';CE = 83;S = 25.98;LAC = 44.05;score =51.01; cluster = 'C'; c_rank =47}
  else if(species == 'Ebenopsis ebano'){ name = 'blackbead ebony';CE = 100;S = 75;LAC = 30.86;score =68.62; cluster = 'B'; c_rank =5}
  else if(species == 'Ehretia anacua'){ name = 'Anacua knockaway';CE = 66.5;S = 50.78;LAC = 46.06;score =54.45; cluster = 'C'; c_rank =36}
  else if(species == 'Eugenia rhombea'){ name = 'red stopper';CE = 74.5;S = 62.66;LAC = 50.62;score =62.59; cluster = 'B'; c_rank =11}
  else if(species == 'Exothea paniculata'){ name = 'Butter bough';CE = 91.5;S = 66.88;LAC = 67.28;score =75.22; cluster = 'A'; c_rank =6}
  else if(species == 'Ficus citrifolia'){ name = 'wild banyantree';CE = 74.75;S = 50;LAC = 39.04;score =54.6; cluster = 'C'; c_rank =35}
  else if(species == 'Fraxinus caroliniana'){ name = 'Carolina ash';CE = 66.25;S = 61.55;LAC = 51.8;score =59.86; cluster = 'A'; c_rank =31}
  else if(species == 'Fraxinus latifolia'){ name = 'Oregon ash';CE = 24.75;S = 63.27;LAC = 33.26;score =40.43; cluster = 'B'; c_rank =43}
  else if(species == 'Fraxinus nigra'){ name = 'black ash ';CE = 57.75;S = 28.21;LAC = 43.06;score =43.01; cluster = 'C'; c_rank =68}
  else if(species == 'Fraxinus pennsylvanica'){ name = 'green ash ';CE = 58;S = 33.65;LAC = 20.36;score =37.34; cluster = 'E1'; c_rank =31}
  else if(species == 'Fraxinus profunda'){ name = 'pumpkin ash';CE = 41.25;S = 59.07;LAC = 57.49;score =52.6; cluster = 'D'; c_rank =13}
  else if(species == 'Fraxinus quadrangulata'){ name = 'blue ash ';CE = 100;S = 45.82;LAC = 44.45;score =63.42; cluster = 'C'; c_rank =9}
  else if(species == 'Fraxinus texensis'){ name = 'Texas ash';CE = 49.5;S = 53.38;LAC = 62.11;score =54.99; cluster = 'D'; c_rank =8}
  else if(species == 'Fraxinus velutina'){ name = 'velvet ash';CE = 24.75;S = 61.76;LAC = 43.77;score =43.43; cluster = 'D'; c_rank =43}
  else if(species == 'Gleditsia aquatica'){ name = 'Water locust';CE = 100;S = 63.09;LAC = 76.36;score =79.81; cluster = 'A'; c_rank =1}
  else if(species == 'Gleditsia triacanthos'){ name = 'honeylocust ';CE = 91.5;S = 17.52;LAC = 56.19;score =55.07; cluster = 'C'; c_rank =31}
  else if(species == 'Gordonia lasianthus'){ name = 'loblolly bay';CE = 0;S = 52.49;LAC = 44.97;score =32.49; cluster = 'E2'; c_rank =25}
  else if(species == 'Guapira discolor'){ name = 'Beef tree';CE = 58;S = 58.5;LAC = 89.93;score =68.81; cluster = 'A'; c_rank =18}
  else if(species == 'Gymnocladus dioicus'){ name = 'Kentucky coffeetree ';CE = 100;S = 66.71;LAC = 46.37;score =71.03; cluster = 'B'; c_rank =3}
  else if(species == 'Halesia carolina'){ name = 'Carolina silver bell';CE = 74.75;S = 68.99;LAC = 59.48;score =67.74; cluster = 'A'; c_rank =20}
  else if(species == 'Halesia diptera'){ name = 'Two-wing';CE = 91.5;S = 70.96;LAC = 65.6;score =76.02; cluster = 'A'; c_rank =5}
  else if(species == 'Juglans californica'){ name = 'southern California black walnut';CE = 41.25;S = 85.02;LAC = 48.52;score =58.26; cluster = 'B'; c_rank =17}
  else if(species == 'Juglans cinerea'){ name = 'butternut ';CE = 83;S = 57.35;LAC = 43.41;score =61.25; cluster = 'B'; c_rank =14}
  else if(species == 'Juglans hindsii'){ name = 'northern California black walnut';CE = 66.25;S = 41.63;LAC = 45.36;score =51.08; cluster = 'C'; c_rank =45}
  else if(species == 'Juglans major'){ name = 'Arizona walnut';CE = 58;S = 75.77;LAC = 47.98;score =60.58; cluster = 'B'; c_rank =15}
  else if(species == 'Juglans microcarpa'){ name = 'Texas walnut';CE = 100;S = 71.52;LAC = 64.4;score =78.64; cluster = 'A'; c_rank =2}
  else if(species == 'Juglans nigra'){ name = 'black walnut ';CE = 83;S = 41.33;LAC = 39.94;score =54.76; cluster = 'C'; c_rank =32}
  else if(species == 'Juniperus ashei'){ name = 'Ashe juniper';CE = 83;S = 33.33;LAC = 65.78;score =60.71; cluster = 'C'; c_rank =16}
  else if(species == 'Juniperus californica'){ name = 'California juniper';CE = 57.75;S = 45.02;LAC = 56.33;score =53.03; cluster = 'C'; c_rank =39}
  else if(species == 'Juniperus coahuilensis'){ name = 'redberry juniper';CE = 74.5;S = 36.77;LAC = 46.77;score =52.68; cluster = 'C'; c_rank =41}
  else if(species == 'Juniperus flaccida'){ name = 'drooping juniper';CE = 33;S = 63.83;LAC = 59.64;score =52.16; cluster = 'D'; c_rank =15}
  else if(species == 'Juniperus pinchotii'){ name = 'Pinchot juniper';CE = 83;S = 24.83;LAC = 62.62;score =56.82; cluster = 'C'; c_rank =25}
  else if(species == 'Juniperus virginiana'){ name = 'eastern redcedar ';CE = 66.25;S = 17.96;LAC = 40.14;score =41.45; cluster = 'E1'; c_rank =27}
  else if(species == 'Laguncularia racemosa'){ name = 'white-mangrove';CE = 49.75;S = 48.98;LAC = 51.49;score =50.07; cluster = 'D'; c_rank =25}
  else if(species == 'Larix laricina'){ name = 'tamarack ';CE = 57.75;S = 36.59;LAC = 32.34;score =42.23; cluster = 'E1'; c_rank =26}
  else if(species == 'Larix lyallii'){ name = 'subalpine larix';CE = 41.25;S = 52.69;LAC = 46.73;score =46.89; cluster = 'D'; c_rank =34}
  else if(species == 'Larix occidentalis'){ name = 'western larix';CE = 0;S = 44.03;LAC = 32.9;score =25.64; cluster = 'E2'; c_rank =42}
  else if(species == 'Leucaena pulverulenta'){ name = 'great leadtree';CE = 66.25;S = 59.27;LAC = 36;score =53.84; cluster = 'B'; c_rank =26}
  else if(species == 'Lithocarpus densiflorus'){ name = 'tanoak';CE = 0;S = 41.79;LAC = 42.42;score =28.07; cluster = 'E2'; c_rank =35}
  else if(species == 'Lysiloma latisiliquum'){ name = 'False tamarind';CE = 24.75;S = 69.44;LAC = 77.35;score =57.18; cluster = 'A'; c_rank =34}
  else if(species == 'Maclura pomifera'){ name = 'Osage-orange';CE = 83;S = 29.89;LAC = 26.31;score =46.4; cluster = 'E1'; c_rank =20}
  else if(species == 'Magnolia acuminata'){ name = 'Cucumber tree';CE = 83;S = 43.73;LAC = 64.05;score =63.59; cluster = 'A'; c_rank =28}
  else if(species == 'Magnolia fraseri'){ name = 'mountain magnolia ';CE = 66.25;S = 43.22;LAC = 56.27;score =55.25; cluster = 'C'; c_rank =30}
  else if(species == 'Magnolia grandiflora'){ name = 'Southern magnolia';CE = 58;S = 44.81;LAC = 50.38;score =51.07; cluster = 'C'; c_rank =46}
  else if(species == 'Magnolia macrophylla'){ name = 'Big leaf magnolia';CE = 91.5;S = 57.19;LAC = 64.7;score =71.13; cluster = 'A'; c_rank =11}
  else if(species == 'Magnolia pyramidata'){ name = 'Pyramid magnolia';CE = 100;S = 65.69;LAC = 67.63;score =77.77; cluster = 'A'; c_rank =4}
  else if(species == 'Magnolia tripetala'){ name = 'Umbrella magnolia';CE = 83.25;S = 57.19;LAC = 60.92;score =67.12; cluster = 'A'; c_rank =22}
  else if(species == 'Malus angustifolia'){ name = 'southern crabapple ';CE = 100;S = 42.83;LAC = 54.16;score =65.66; cluster = 'C'; c_rank =4}
  else if(species == 'Malus coronaria'){ name = 'Sweet crabapple';CE = 100;S = 44.73;LAC = 64.33;score =69.69; cluster = 'A'; c_rank =14}
  else if(species == 'Malus fusca'){ name = 'Oregon crab apple';CE = 0;S = 54.92;LAC = 41.95;score =32.29; cluster = 'E2'; c_rank =26}
  else if(species == 'Malus ioensis'){ name = 'prairie crabapple';CE = 100;S = 40.89;LAC = 46.35;score =62.41; cluster = 'C'; c_rank =11}
  else if(species == 'Metopium toxiferum'){ name = 'Florida poisontree';CE = 0;S = 47.81;LAC = 70.37;score =39.39; cluster = 'D'; c_rank =49}
  else if(species == 'Morus rubra'){ name = 'red mulberry ';CE = 91.5;S = 29.42;LAC = 48.65;score =56.52; cluster = 'C'; c_rank =27}
  else if(species == 'Nyssa aquatica'){ name = 'water tupelo ';CE = 66.25;S = 32.6;LAC = 60.18;score =53.01; cluster = 'C'; c_rank =40}
  else if(species == 'Nyssa ogeche'){ name = 'Ogeechee tupelo';CE = 49.75;S = 45.29;LAC = 48.62;score =47.89; cluster = 'C'; c_rank =56}
  else if(species == 'Olneya tesota'){ name = 'desert ironwood';CE = 24.75;S = 49.96;LAC = 54.79;score =43.17; cluster = 'D'; c_rank =45}
  else if(species == 'Ostrya virginiana'){ name = 'eastern hophornbeam ';CE = 66.25;S = 21.85;LAC = 25.61;score =37.9; cluster = 'E1'; c_rank =30}
  else if(species == 'Oxydendrum arboreum'){ name = 'sourwood ';CE = 49.5;S = 61.56;LAC = 37.64;score =49.57; cluster = 'B'; c_rank =32}
  else if(species == 'Persea borbonia'){ name = 'redbay ';CE = 33.25;S = 41.5;LAC = 70.68;score =48.48; cluster = 'D'; c_rank =28}
  else if(species == 'Picea breweriana'){ name = 'Brewer spruce';CE = 49.5;S = 73.36;LAC = 47.79;score =56.88; cluster = 'B'; c_rank =18}
  else if(species == 'Picea engelmannii'){ name = 'Engelmann spruce';CE = 0;S = 44.24;LAC = 36.26;score =26.83; cluster = 'E2'; c_rank =38}
  else if(species == 'Picea glauca'){ name = 'white spruce';CE = 58;S = 42.13;LAC = 47.6;score =49.24; cluster = 'C'; c_rank =50}
  else if(species == 'Picea mariana'){ name = 'black spruce';CE = 49.5;S = 40.92;LAC = 47.44;score =45.96; cluster = 'C'; c_rank =60}
  else if(species == 'Picea pungens'){ name = 'blue spruce';CE = 41.5;S = 59.57;LAC = 48.22;score =49.76; cluster = 'B'; c_rank =31}
  else if(species == 'Picea rubens'){ name = 'red spruce ';CE = 8.25;S = 42.03;LAC = 63.85;score =38.05; cluster = 'D'; c_rank =52}
  else if(species == 'Picea sitchensis'){ name = 'Sitka spruce';CE = 0;S = 57.42;LAC = 48.6;score =35.34; cluster = 'E2'; c_rank =16}
  else if(species == 'Pinus aristata'){ name = 'Bristlecone pine (Rocky Mountain)';CE = 24.75;S = 40.89;LAC = 57.93;score =41.19; cluster = 'D'; c_rank =47}
  else if(species == 'Pinus arizonica'){ name = 'Arizona pine';CE = 49.5;S = 57.55;LAC = 45.68;score =50.91; cluster = 'B'; c_rank =29}
  else if(species == 'Pinus balfouriana'){ name = 'foxtail pine';CE = 49.5;S = 51.24;LAC = 52.94;score =51.23; cluster = 'D'; c_rank =18}
  else if(species == 'Pinus banksiana'){ name = 'jack pine';CE = 57.75;S = 44.84;LAC = 44.89;score =49.16; cluster = 'C'; c_rank =51}
  else if(species == 'Pinus cembroides'){ name = 'Mexican pinyon pine';CE = 66;S = 49.27;LAC = 47.71;score =54.33; cluster = 'C'; c_rank =37}
  else if(species == 'Pinus clausa'){ name = 'sand pine';CE = 41.5;S = 58.46;LAC = 52.68;score =50.88; cluster = 'D'; c_rank =20}
  else if(species == 'Pinus contorta'){ name = 'lodgepole pine';CE = 0;S = 35.78;LAC = 41.86;score =25.88; cluster = 'E2'; c_rank =41}
  else if(species == 'Pinus coulteri'){ name = 'Coulter pine';CE = 41.25;S = 78.1;LAC = 47.83;score =55.73; cluster = 'B'; c_rank =23}
  else if(species == 'Pinus discolor'){ name = 'border pinyon';CE = 49.5;S = 49.27;LAC = 49.26;score =49.34; cluster = 'C'; c_rank =49}
  else if(species == 'Pinus engelmannii'){ name = 'Apache pine';CE = 33;S = 57.55;LAC = 62.38;score =50.98; cluster = 'D'; c_rank =19}
  else if(species == 'Pinus glabra'){ name = 'spruce pine ';CE = 66.25;S = 57.42;LAC = 43.84;score =55.84; cluster = 'B'; c_rank =21}
  else if(species == 'Pinus lambertiana'){ name = 'sugar pine';CE = 0;S = 39.52;LAC = 41.91;score =27.14; cluster = 'E2'; c_rank =37}
  else if(species == 'Pinus leiophylla'){ name = 'Chihuahuan pine';CE = 33;S = 55.9;LAC = 57.12;score =48.67; cluster = 'D'; c_rank =26}
  else if(species == 'Pinus longaeva'){ name = 'bristlecone pine (Great Basin)';CE = 57.75;S = 40.89;LAC = 58.21;score =52.29; cluster = 'C'; c_rank =43}
  else if(species == 'Pinus monticola'){ name = 'western white pine';CE = 8.25;S = 46.67;LAC = 39.67;score =31.53; cluster = 'E2'; c_rank =28}
  else if(species == 'Pinus muricata'){ name = 'bishop pine';CE = 24.75;S = 61.59;LAC = 55.63;score =47.33; cluster = 'D'; c_rank =33}
  else if(species == 'Pinus pungens'){ name = 'Table Mountain pine ';CE = 83.25;S = 52.44;LAC = 48.1;score =61.26; cluster = 'C'; c_rank =14}
  else if(species == 'Pinus quadrifolia'){ name = 'four-leaf pine';CE = 33;S = 74.19;LAC = 62.59;score =56.59; cluster = 'D'; c_rank =5}
  else if(species == 'Pinus radiata'){ name = 'Monterey pine';CE = 91.5;S = 60.23;LAC = 53.49;score =68.41; cluster = 'B'; c_rank =6}
  else if(species == 'Pinus remota'){ name = 'papershell pinyon pine';CE = 49.5;S = 49.27;LAC = 61.66;score =53.48; cluster = 'D'; c_rank =9}
  else if(species == 'Pinus resinosa'){ name = 'red pine ';CE = 49.5;S = 39.9;LAC = 43.26;score =44.22; cluster = 'C'; c_rank =63}
  else if(species == 'Pinus rigida'){ name = 'pitch pine ';CE = 66.25;S = 41.01;LAC = 44.56;score =50.61; cluster = 'C'; c_rank =48}
  else if(species == 'Pinus strobiformis'){ name = 'southwestern white pine';CE = 33;S = 79.42;LAC = 48.88;score =53.76; cluster = 'B'; c_rank =27}
  else if(species == 'Pinus torreyana'){ name = 'Torrey pine';CE = 49.5;S = 66.1;LAC = 53.68;score =56.43; cluster = 'B'; c_rank =19}
  else if(species == 'Pinus virginiana'){ name = 'Virginia pine ';CE = 49.5;S = 24.25;LAC = 26.15;score =33.3; cluster = 'E1'; c_rank =36}
  else if(species == 'Pinus washoensis'){ name = 'Washoe pine';CE = 16.5;S = 57.76;LAC = 67.76;score =47.34; cluster = 'D'; c_rank =32}
  else if(species == 'Piscidia piscipula'){ name = 'Fish poison tree';CE = 41.25;S = 53.42;LAC = 77.49;score =57.39; cluster = 'A'; c_rank =33}
  else if(species == 'Planera aquatica'){ name = 'water-elm';CE = 91.5;S = 44.81;LAC = 41.03;score =59.11; cluster = 'C'; c_rank =21}
  else if(species == 'Platanus occidentalis'){ name = 'American sycamore ';CE = 66.25;S = 38.52;LAC = 17.72;score =40.83; cluster = 'E1'; c_rank =29}
  else if(species == 'Platanus racemosa'){ name = 'California sycamore';CE = 41.25;S = 57.51;LAC = 31.78;score =43.51; cluster = 'B'; c_rank =40}
  else if(species == 'Platanus wrightii'){ name = 'Arizona sycamore';CE = 24.75;S = 60.94;LAC = 21.4;score =35.7; cluster = 'E2'; c_rank =14}
  else if(species == 'Populus balsamifera'){ name = 'balsam poplar';CE = 57.75;S = 16.33;LAC = 37.8;score =37.29; cluster = 'E1'; c_rank =32}
  else if(species == 'Populus deltoides'){ name = 'eastern cottonwood ';CE = 100;S = 21.64;LAC = 32.98;score =51.54; cluster = 'E1'; c_rank =10}
  else if(species == 'Populus fremontii'){ name = 'Fremont cottonwood';CE = 58.25;S = 42.27;LAC = 43.59;score =48.04; cluster = 'C'; c_rank =54}
  else if(species == 'Populus grandidentata'){ name = 'bigtooth aspen ';CE = 41.25;S = 24.01;LAC = 54.38;score =39.88; cluster = 'C'; c_rank =69}
  else if(species == 'Populus heterophylla'){ name = 'swamp cottonwood';CE = 83;S = 49.54;LAC = 48.5;score =60.34; cluster = 'C'; c_rank =18}
  else if(species == 'Prosopis glandulosa'){ name = 'honey mequite';CE = 57.75;S = 4.13;LAC = 19.68;score =27.18; cluster = 'E1'; c_rank =37}
  else if(species == 'Prosopis pubescens'){ name = 'screwbean mesquite';CE = 58.25;S = 56.96;LAC = 38.75;score =51.32; cluster = 'B'; c_rank =28}
  else if(species == 'Prunus americana'){ name = 'American plum ';CE = 100;S = 29.29;LAC = 40.15;score =56.48; cluster = 'E1'; c_rank =4}
  else if(species == 'Prunus angustifolia'){ name = 'Chickasaw plum';CE = 91.5;S = 37.66;LAC = 36.01;score =55.06; cluster = 'E1'; c_rank =5}
  else if(species == 'Prunus nigra'){ name = 'Canada plum';CE = 33.25;S = 50.17;LAC = 52.95;score =45.45; cluster = 'D'; c_rank =40}
  else if(species == 'Prunus pensylvanica'){ name = 'pin cherry ';CE = 66.25;S = 24.71;LAC = 32.52;score =41.16; cluster = 'E1'; c_rank =28}
  else if(species == 'Prunus virginiana'){ name = 'chokecherry ';CE = 83;S = 21.69;LAC = 37.21;score =47.3; cluster = 'E1'; c_rank =18}
  else if(species == 'Pseudotsuga macrocarpa'){ name = 'bigcone Douglas-fir';CE = 66.25;S = 56.82;LAC = 44.4;score =55.82; cluster = 'B'; c_rank =22}
  else if(species == 'Pseudotsuga menziesii'){ name = 'Douglas-fir';CE = 0;S = 35.78;LAC = 36.05;score =23.94; cluster = 'E2'; c_rank =43}
  else if(species == 'Quercus alba'){ name = 'white oak ';CE = 24.75;S = 44.58;LAC = 49.05;score =39.46; cluster = 'E2'; c_rank =5}
  else if(species == 'Quercus arizonica'){ name = 'Arizona white oak';CE = 41.25;S = 59.15;LAC = 54.76;score =51.72; cluster = 'D'; c_rank =17}
  else if(species == 'Quercus arkansana'){ name = 'Arkansas oak';CE = 83.25;S = 66.54;LAC = 59.6;score =69.8; cluster = 'A'; c_rank =13}
  else if(species == 'Quercus bicolor'){ name = 'Swamp white oak';CE = 74.75;S = 50.9;LAC = 57.76;score =61.14; cluster = 'A'; c_rank =30}
  else if(species == 'Quercus boyntonii'){ name = 'Boynton oak ';CE = 83.25;S = 65.46;LAC = 49.07;score =65.92; cluster = 'B'; c_rank =9}
  else if(species == 'Quercus ellipsoidalis'){ name = 'northern pin oak';CE = 74.5;S = 16.5;LAC = 53.01;score =48; cluster = 'C'; c_rank =55}
  else if(species == 'Quercus emoryi'){ name = 'Emory oak';CE = 41.25;S = 35.31;LAC = 55.74;score =44.1; cluster = 'C'; c_rank =64}
  else if(species == 'Quercus engelmannii'){ name = 'Engelmann oak';CE = 24.75;S = 70.75;LAC = 55.32;score =50.27; cluster = 'D'; c_rank =24}
  else if(species == 'Quercus garryana'){ name = 'Oregon white oak';CE = 0;S = 53.27;LAC = 25.53;score =26.27; cluster = 'E2'; c_rank =40}
  else if(species == 'Quercus graciliformis'){ name = 'slender oak';CE = 49.75;S = 52.96;LAC = 57.21;score =53.31; cluster = 'D'; c_rank =10}
  else if(species == 'Quercus gravesii'){ name = 'Graves oak';CE = 57.75;S = 56.14;LAC = 53.46;score =55.78; cluster = 'D'; c_rank =6}
  else if(species == 'Quercus grisea'){ name = 'gray oak';CE = 33;S = 68.64;LAC = 43.92;score =48.52; cluster = 'B'; c_rank =35}
  else if(species == 'Quercus hypoleucoides'){ name = 'silverleaf oak';CE = 16.5;S = 68.64;LAC = 41.11;score =42.08; cluster = 'E2'; c_rank =2}
  else if(species == 'Quercus ilicifolia'){ name = 'scrub oak ';CE = 49.75;S = 33;LAC = 47.9;score =43.55; cluster = 'C'; c_rank =66}
  else if(species == 'Quercus imbricaria'){ name = 'shingle oak ';CE = 91.5;S = 32.91;LAC = 44.48;score =56.3; cluster = 'C'; c_rank =28}
  else if(species == 'Quercus incana'){ name = 'bluejack oak';CE = 91.5;S = 54.81;LAC = 53.72;score =66.68; cluster = 'B'; c_rank =8}
  else if(species == 'Quercus kelloggii'){ name = 'Calfornia black oak';CE = 0;S = 58.33;LAC = 53.48;score =37.27; cluster = 'E2'; c_rank =11}
  else if(species == 'Quercus laceyi'){ name = 'Lacey oak';CE = 49.5;S = 72.77;LAC = 54.2;score =58.82; cluster = 'A'; c_rank =32}
  else if(species == 'Quercus laevis'){ name = 'turkey oak';CE = 49.5;S = 49.83;LAC = 32.7;score =44.01; cluster = 'B'; c_rank =39}
  else if(species == 'Quercus lobata'){ name = 'California white oak';CE = 16.5;S = 45.71;LAC = 38.69;score =33.63; cluster = 'E2'; c_rank =22}
  else if(species == 'Quercus lyrata'){ name = 'overcup oak ';CE = 57.75;S = 42.91;LAC = 55.05;score =51.91; cluster = 'C'; c_rank =44}
  else if(species == 'Quercus macrocarpa'){ name = 'bur oak ';CE = 83;S = 39.44;LAC = 40.21;score =54.22; cluster = 'E1'; c_rank =7}
  else if(species == 'Quercus margarettiae'){ name = 'dwarf post oak';CE = 91.5;S = 45.7;LAC = 41.61;score =59.6; cluster = 'C'; c_rank =19}
  else if(species == 'Quercus marilandica'){ name = 'blackjack oak ';CE = 91.5;S = 15.89;LAC = 32.23;score =46.54; cluster = 'E1'; c_rank =19}
  else if(species == 'Quercus michauxii'){ name = 'swamp chestnut oak ';CE = 57.75;S = 53.1;LAC = 59.23;score =56.69; cluster = 'D'; c_rank =4}
  else if(species == 'Quercus minima'){ name = 'dwarf live oak';CE = 0;S = 65.48;LAC = 57.16;score =40.88; cluster = 'E2'; c_rank =4}
  else if(species == 'Quercus muehlenbergii'){ name = 'chinkapin oak ';CE = 83;S = 40.89;LAC = 54.67;score =59.52; cluster = 'C'; c_rank =20}
  else if(species == 'Quercus oblongifolia'){ name = 'Mexican blue oak';CE = 33;S = 62.38;LAC = 64.05;score =53.14; cluster = 'D'; c_rank =12}
  else if(species == 'Quercus pagoda'){ name = 'cherrybark oak ';CE = 57.75;S = 32.23;LAC = 47.64;score =45.87; cluster = 'C'; c_rank =61}
  else if(species == 'Quercus palustris'){ name = 'pin oak ';CE = 83;S = 32.91;LAC = 48.1;score =54.67; cluster = 'C'; c_rank =33}
  else if(species == 'Quercus polymorpha'){ name = 'Mexican white oak';CE = 41.25;S = 85.39;LAC = 54.76;score =60.47; cluster = 'B'; c_rank =16}
  else if(species == 'Quercus prinoides'){ name = 'dwarf chinquapin oak';CE = 91.5;S = 54.2;LAC = 54.84;score =66.85; cluster = 'C'; c_rank =2}
  else if(species == 'Quercus prinus'){ name = 'chestnut oak ';CE = 49.5;S = 52.15;LAC = 46.23;score =49.29; cluster = 'B'; c_rank =33}
  else if(species == 'Quercus rugosa'){ name = 'netleaf oak';CE = 33;S = 77.02;LAC = 52.88;score =54.3; cluster = 'B'; c_rank =24}
  else if(species == 'Quercus shumardii'){ name = 'Shumard oak ';CE = 100;S = 45.02;LAC = 48.57;score =64.53; cluster = 'C'; c_rank =7}
  else if(species == 'Quercus similis'){ name = 'Delta post oak ';CE = 66.25;S = 33.2;LAC = 45.56;score =48.34; cluster = 'C'; c_rank =53}
  else if(species == 'Quercus sinuata var. sinuata'){ name = 'Durand oak ';CE = 83.25;S = 33.2;LAC = 47.45;score =54.63; cluster = 'C'; c_rank =34}
  else if(species == 'Quercus texana'){ name = 'Nutall oak';CE = 57.75;S = 62.54;LAC = 48.01;score =56.1; cluster = 'B'; c_rank =20}
  else if(species == 'Quercus velutina'){ name = 'black oak ';CE = 49.5;S = 22.6;LAC = 30.76;score =34.29; cluster = 'E1'; c_rank =34}
  else if(species == 'Quercus virginiana'){ name = 'live oak';CE = 33;S = 56.27;LAC = 47.9;score =45.72; cluster = 'D'; c_rank =38}
  else if(species == 'Rhizophora mangle'){ name = 'American mangrove';CE = 41.25;S = 54.04;LAC = 49.6;score =48.3; cluster = 'D'; c_rank =29}
  else if(species == 'Robinia neomexicana'){ name = 'New Mexico locust';CE = 41.25;S = 46.56;LAC = 47.18;score =45; cluster = 'D'; c_rank =41}
  else if(species == 'Robinia pseudoacacia'){ name = 'black locust ';CE = 74.5;S = 12.38;LAC = 52.62;score =46.5; cluster = 'C'; c_rank =59}
  else if(species == 'Sabal mexicana'){ name = 'Mexican palmetto';CE = 91.5;S = 63.83;LAC = 65.4;score =73.58; cluster = 'A'; c_rank =8}
  else if(species == 'Sabal palmetto'){ name = 'cabbage palmetto';CE = 8.25;S = 44.81;LAC = 50.71;score =34.59; cluster = 'E2'; c_rank =18}
  else if(species == 'Salix amygdaloides'){ name = 'peachleaf willow';CE = 100;S = 28.83;LAC = 41.08;score =56.64; cluster = 'E1'; c_rank =3}
  else if(species == 'Salix bebbiana'){ name = 'Bebb willow';CE = 91.5;S = 29.21;LAC = 51.76;score =57.49; cluster = 'C'; c_rank =22}
  else if(species == 'Salix bonplandiana'){ name = 'Bonpland willow';CE = 33;S = 49.58;LAC = 49.39;score =43.99; cluster = 'D'; c_rank =42}
  else if(species == 'Salix caroliniana'){ name = 'coastal plain willow';CE = 100;S = 34.02;LAC = 51.71;score =61.91; cluster = 'C'; c_rank =12}
  else if(species == 'Salix nigra'){ name = 'black willow ';CE = 91.5;S = 28.1;LAC = 39.53;score =53.04; cluster = 'E1'; c_rank =9}
  else if(species == 'Salix scouleriana'){ name = 'Scoulers willow';CE = 24.75;S = 44.5;LAC = 48.38;score =39.21; cluster = 'D'; c_rank =50}
  else if(species == 'Sapindus saponaria var. drummondii'){ name = 'western soapberry';CE = 83.25;S = 70.1;LAC = 53.32;score =68.89; cluster = 'B'; c_rank =4}
  else if(species == 'Sassafras albidum'){ name = 'sassafras ';CE = 57.75;S = 11.77;LAC = 38.9;score =36.14; cluster = 'E1'; c_rank =33}
  else if(species == 'Sequoia sempervirens'){ name = 'coast redwood';CE = 8.25;S = 78.31;LAC = 51.67;score =46.08; cluster = 'E2'; c_rank =1}
  else if(species == 'Sequoiadendron giganteum'){ name = 'giant sequoia';CE = 49.75;S = 62.82;LAC = 49.23;score =53.93; cluster = 'B'; c_rank =25}
  else if(species == 'Sideroxylon'){ name = 'False mastic';CE = 41.25;S = 50;LAC = 75.11;score =55.45; cluster = 'A'; c_rank =35}
  else if(species == 'Sideroxylon lanuginosum subsp. lanuginosum'){ name = 'gum bumelia ';CE = 100;S = 21.66;LAC = 60.4;score =60.69; cluster = 'C'; c_rank =17}
  else if(species == 'Sideroxylon salicifolium'){ name = 'white bully';CE = 16.5;S = 45.75;LAC = 63.54;score =41.93; cluster = 'D'; c_rank =46}
  else if(species == 'Simarouba glauca'){ name = 'Paradise-tree';CE = 66;S = 70.28;LAC = 77.15;score =71.14; cluster = 'A'; c_rank =10}
  else if(species == 'Sorbus americana'){ name = 'American mountain-ash ';CE = 49.5;S = 42.67;LAC = 44.35;score =45.5; cluster = 'C'; c_rank =62}
  else if(species == 'Sorbus decora'){ name = 'northern mountain-ash';CE = 100;S = 62.24;LAC = 51.02;score =71.09; cluster = 'B'; c_rank =2}
  else if(species == 'Taxodium distichum'){ name = 'baldcypress ';CE = 58;S = 38.42;LAC = 31.32;score =42.58; cluster = 'E1'; c_rank =24}
  else if(species == 'Taxus brevifolia'){ name = 'Pacific yew';CE = 8.25;S = 41.79;LAC = 29.67;score =26.57; cluster = 'E2'; c_rank =39}
  else if(species == 'Thrinax radiata'){ name = 'Florida thatch palm';CE = 16.5;S = 59.28;LAC = 30.86;score =35.55; cluster = 'E2'; c_rank =15}
  else if(species == 'Thuja plicata'){ name = 'western redcedar';CE = 0;S = 44.81;LAC = 41.48;score =28.76; cluster = 'E2'; c_rank =34}
  else if(species == 'Tilia americana'){ name = 'American basswood ';CE = 66.25;S = 40.11;LAC = 56.17;score =54.18; cluster = 'C'; c_rank =38}
  else if(species == 'Tilia americana var. caroliniana'){ name = 'Carolina basswood ';CE = 58.25;S = 53.51;LAC = 58.81;score =56.85; cluster = 'D'; c_rank =2}
  else if(species == 'Tilia americana var. heterophylla'){ name = 'White basswood';CE = 83.25;S = 64.99;LAC = 58.81;score =69.01; cluster = 'A'; c_rank =17}
  else if(species == 'Torreya californica'){ name = 'California torreya (nutmeg)';CE = 8.25;S = 50.04;LAC = 58.23;score =38.84; cluster = 'D'; c_rank =51}
  else if(species == 'Tsuga caroliniana'){ name = 'Carolina hemlock ';CE = 66.25;S = 90.69;LAC = 46.04;score =67.66; cluster = 'B'; c_rank =7}
  else if(species == 'Tsuga heterophylla'){ name = 'western hemlock';CE = 0;S = 44.24;LAC = 45.78;score =30.01; cluster = 'E2'; c_rank =31}
  else if(species == 'Tsuga mertensiana'){ name = 'mountain hemlock';CE = 8.25;S = 56.61;LAC = 45.95;score =36.94; cluster = 'E2'; c_rank =12}
  else if(species == 'Ulmus americana'){ name = 'American elm ';CE = 58;S = 23.02;LAC = 20.69;score =33.91; cluster = 'E1'; c_rank =35}
  else if(species == 'Ulmus crassifolia'){ name = 'cedar elm';CE = 74.75;S = 47.2;LAC = 49.69;score =57.21; cluster = 'C'; c_rank =24}
  else if(species == 'Ulmus rubra'){ name = 'slippery elm ';CE = 74.75;S = 41.22;LAC = 30.5;score =48.82; cluster = 'E1'; c_rank =15}
  else if(species == 'Ulmus serotina'){ name = 'September elm';CE = 91.5;S = 70.22;LAC = 57.87;score =73.2; cluster = 'A'; c_rank =9}
  else if(species == 'Ulmus thomasii'){ name = 'rock elm ';CE = 100;S = 47.47;LAC = 53.75;score =67.08; cluster = 'C'; c_rank =1}
  else if(species == 'Umbellularia californica'){ name = 'California-laurel';CE = 8.25;S = 41.37;LAC = 54.72;score =34.78; cluster = 'E2'; c_rank =17}
  else if(species == 'Yucca brevifolia'){ name = 'Joshua tree';CE = 41.5;S = 61.76;LAC = 44.53;score =49.26; cluster = 'B'; c_rank =34}
  else if(species == 'NA'){name = 'Tree unknown';CE = NA;S = NA;LAC = NA;score =NA; cluster = NA; c_rank =NA} #unknown/ na action
  else{name = 'Tree unknown';CE = NA;S = NA;LAC = NA;score =NA; cluster = NA; c_rank =NA} 
  
  all_info = c(name = name,CE = CE,S = S,LAC = LAC,score = score,cluster = cluster,c_rank = c_rank)
  
  if(desired_return == "score") return(score)
  else if(desired_return == "all") return(all_info)
}

####   basic test and how to use   ####
#if you've scrolled this far this may help
#test individual species
x1 = c('Salix nigra')
x2 = c('Yucca brevifolia')
x3 = c('Unknown') # a species not within data will return NA
VCC(x1,'score')
VCC(x2,'score')
VCC(x3,'score')# a species not within data will return NA
VCC(x2,'all')# list formatted data for extraction of auxiliary data
VCC(x2,'all')[6]# item comprehension to pull cluster value of tree x2

#dealing with lists as dataframes
VCC_test_data = c('Salix nigra','Yucca brevifolia','Quercus bicolor','Pinus coulteri') #create fake tree list
VCC_test_data = as.data.frame(VCC_test_data) #as a datafram
names(VCC_test_data) = c('species') #change column name
VCC_test_data$score = apply(X=VCC_test_data,MARGIN = 1,FUN = VCC,'score')# apply function over dataframe rows to get scores appended to tree records 
VCC_test_data$cluster= apply(X=VCC_test_data,MARGIN = 1,FUN = VCC,'all')[6,] # apply function and list comprehension over dataframe rows to get cluster appended to tree records 
print(VCC_test_data)

rm(x1,x2,x3,VCC_test_data)

#apply over dataframe with many fields can sometimes require first subsetting to field with species data 
your_data = read.csv("C:/your/directory/to/your_data.csv") #read in your dataframe e.g excel
your_data$VCC_score = apply(X = your_data[c("Scientific_Name")],MARGIN = 1,FUN = VCC,'score') # passes only the field containing the scientific name though function
your_data$VCC_cluster = apply(X = your_data[c("Scientific_Name")],MARGIN = 1,FUN = VCC,'all')[6,] # same thing but returns cluster using list comprehension



