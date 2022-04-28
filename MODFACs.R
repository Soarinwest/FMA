###########################################################################################################################################################
# Title: 
# Writer: Soren Donisvitch 
# Date 4/20/22
# Cited material: 
#
# Contact: soren.donisvitch@gmail.com
# Requirements: Requires tree species names to be formatted as scientific name (Genus species) with capitalized genus and lowercase species
# Complementary functions: FIA_species_translation (translates FIA and scientific names of common North American Tree species)
###########################################################################################################################################################

# Tree species vulnerability to climate change (VCC) function produces primary scores and auxiliary info from Potter, 2017,. CAPTURE framework
# Inputs: 

MODFACs = function(species){
  if(is.na(species)){S_Code = 'NA';CMIV = NA;CHHADhi =NA;DFS =NA;BFS =NA;EVS = NA;NCS = NA;LDES = NA;DREkm =NA} #unknown/ na action first to avoid missing value where true/false needed  
  else if(species == 'Abies balsamea'){S_Code = 'ABBA';CMIV = 9519;CHHADhi =-86;DFS =-3;BFS = -0.35;EVS = 2.29;NCS = -0.09;LDES = NA ;DREkm = NA}
  else if(species == 'Chamaecyparis thyoides'){S_Code = 'CHTH';CMIV = 167;CHHADhi =-7.8;DFS =-0.61;BFS = -1.21;EVS = 2.37;NCS = -2.37;LDES = -0.34;DREkm = NA}
  else if(species == 'Juniperus virginiana'){S_Code = 'JUVI';CMIV = 13509;CHHADhi =105;DFS =0.56;BFS = -1.48;EVS = -0.24;NCS = 0.79;LDES = 2.16;DREkm = 170.88}
  else if(species == 'Larix laricina'){S_Code = 'LALA';CMIV = 2233;CHHADhi =-53.7;DFS =-0.48;BFS = -1.24;EVS = 1.74;NCS = -3;LDES = 2.91;DREkm = 84.86}
  else if(species == 'Picea glauca'){S_Code = 'PIGL';CMIV = 1520;CHHADhi =-60.6;DFS =0.07;BFS = -0.62;EVS = 1.74;NCS = -1.74;LDES = 2.68;DREkm = 254.6}
  else if(species == 'Picea mariana'){S_Code = 'PIMA';CMIV = 3176;CHHADhi =-95.8;DFS =-2.14;BFS = 1.24;EVS = 2.76;NCS = -3;LDES = 3;DREkm = NA}
  else if(species == 'Picea rubens'){S_Code = 'PIRU';CMIV = 2875;CHHADhi =-62.4;DFS =-1.28;BFS = -0.62;EVS = 2.37;NCS = 0.66;LDES = 2.48;DREkm = NA}
  else if(species == 'Pinus banksiana'){S_Code = 'PIBA';CMIV = 3192;CHHADhi =-43;DFS =1.87;BFS = -1.24;EVS = 1.11;NCS = -2.68;LDES = 1.67;DREkm = 700}
  else if(species == 'Pinus clausa'){S_Code = 'PICL';CMIV = 875;CHHADhi =153.1;DFS =-1.09;BFS = -1.14;EVS = 0.39;NCS = -2.94;LDES = -1.35;DREkm = 458.4}
  else if(species == 'Pinus echinata'){S_Code = 'PIEC';CMIV = 10087;CHHADhi =181.4;DFS =0;BFS = -0.97;EVS = -0.63;NCS = -0.22;LDES = 1.6;DREkm = 254.6}
  else if(species == 'Pinus elliottii'){S_Code = 'PIEL';CMIV = 14744;CHHADhi =56.2;DFS =1.14;BFS = -1.67;EVS = -1.89;NCS = -2.49;LDES = 0.15;DREkm = 424.2}
  else if(species == 'Pinus glabra'){S_Code = 'PIGL2';CMIV = 238;CHHADhi =152.5;DFS =0.51;BFS = -0.28;EVS = 2.45;NCS = -2.49;LDES = -0.3;DREkm = 322.4}
  else if(species == 'Pinus palustris'){S_Code = 'PIPA';CMIV = 4849;CHHADhi =66.9;DFS =0.96;BFS = -1.74;EVS = -1.18;NCS = -2.12;LDES = 0.83;DREkm = 305.2}
  else if(species == 'Pinus pungens'){S_Code = 'PIPU';CMIV = 100;CHHADhi =64;DFS =2.55;BFS = -1.12;EVS = 1.97;NCS = -2.87;LDES = 1.59;DREkm = NA}
  else if(species == 'Pinus resinosa'){S_Code = 'PIRE';CMIV = 3421;CHHADhi =-40.9;DFS =-0.03;BFS = -2.64;EVS = 0.32;NCS = -1.93;LDES = 2.91;DREkm = 128.04}
  else if(species == 'Pinus rigida'){S_Code = 'PIRI';CMIV = 1203;CHHADhi =-20.4;DFS =0.56;BFS = -1.79;EVS = 2.53;NCS = 2.62;LDES = 2.14;DREkm = NA}
  else if(species == 'Pinus serotina'){S_Code = 'PISE';CMIV = 1158;CHHADhi =-26.6;DFS =-1.06;BFS = -1.51;EVS = 2.45;NCS = -2.18;LDES = 2.13;DREkm = 323}
  else if(species == 'Pinus strobus'){S_Code = 'PIST';CMIV = 8628;CHHADhi =-55.2;DFS =-1.97;BFS = 0.13;EVS = -1.18;NCS = 2.56;LDES = 2.58;DREkm = 152.28}
  else if(species == 'Pinus taeda'){S_Code = 'PITA';CMIV = 46705;CHHADhi =34;DFS =-0.51;BFS = -0.66;EVS = -2.53;NCS = -0.92;LDES = 0.96;DREkm = 412.4}
  else if(species == 'Pinus virginiana'){S_Code = 'PIVA';CMIV = 5817;CHHADhi =-28.6;DFS =0.1;BFS = -0.81;EVS = 0.47;NCS = -0.28;LDES = 1.67;DREkm = 208.8}
  else if(species == 'Taxodium distichum'){S_Code = 'TADI';CMIV = 3014;CHHADhi =57.9;DFS =0.38;BFS = -1.02;EVS = 1.18;NCS = -2.37;LDES = 2.29;DREkm = 323}
  else if(species == 'Taxodium distichum N'){S_Code = 'TADN';CMIV = 3335;CHHADhi =212.9;DFS =1.56;BFS = -1.05;EVS = -3;NCS = -3;LDES = -1.54;DREkm = 685.8}
  else if(species == 'Thuja occidentalis'){S_Code = 'THOC';CMIV = 4672;CHHADhi =-78.4;DFS =-0.7;BFS = 0.49;EVS = 2.13;NCS = -3;LDES = 3;DREkm = NA}
  else if(species == 'Tsuga canadensis'){S_Code = 'TSCA';CMIV = 5951;CHHADhi =-48.2;DFS =-1.34;BFS = -0.88;EVS = 0.63;NCS = 3;LDES = 2.9;DREkm = 130.32}
  else if(species == 'Acer barbatum'){S_Code = 'ACBA';CMIV = 187;CHHADhi =11.8;DFS =1.84;BFS = 0.68;EVS = 2.37;NCS = -2.18;LDES = -1.44;DREkm = NA}
  else if(species == 'Acer negundo'){S_Code = 'ACNE';CMIV = 14132;CHHADhi =84.4;DFS =2.39;BFS = 2.06;EVS = -1.89;NCS = 0.73;LDES = 2.64;DREkm = 141.4}
  else if(species == 'Acer nigrum'){S_Code = 'ACNI';CMIV = 181;CHHADhi =-62.4;DFS =0.48;BFS = 0.9;EVS = 2.76;NCS = -3;LDES = 1.94;DREkm = NA}
  else if(species == 'Acer pensylvanicum'){S_Code = 'ACPE';CMIV = 2081;CHHADhi =-67.2;DFS =0.96;BFS = 0.25;EVS = 1.26;NCS = -0.28;LDES = 2.84;DREkm = NA}
  else if(species == 'Acer rubrum'){S_Code = 'ACRU';CMIV = 47858;CHHADhi =-40.9;DFS =3;BFS = 3;EVS = -0.24;NCS = 0.85;LDES = 2.97;DREkm = NA}
  else if(species == 'Acer saccharinum'){S_Code = 'ACSA2';CMIV = 7835;CHHADhi =116.4;DFS =0.13;BFS = 1.63;EVS = -1.26;NCS = 1.17;LDES = 1.94;DREkm = 178.88}
  else if(species == 'Acer saccharum'){S_Code = 'ACSA ';CMIV = 27948;CHHADhi =-73.4;DFS =0.86;BFS = 1.34;EVS = -0.16;NCS = 2.37;LDES = 2.71;DREkm = 244}
  else if(species == 'Acer spicatum'){S_Code = 'ACSP';CMIV = 515;CHHADhi =-95;DFS =0.8;BFS = 1.48;EVS = 2.53;NCS = 0.16;LDES = 0.67;DREkm = NA}
  else if(species == 'Aesculus glabra'){S_Code = 'AEGL';CMIV = 549;CHHADhi =-22.6;DFS =0.38;BFS = -1.92;EVS = 1.66;NCS = 0.35;LDES = -1.14;DREkm = 573}
  else if(species == 'Aesculus octandra'){S_Code = 'AEOC';CMIV = 397;CHHADhi =-2;DFS =-0.03;BFS = -2.13;EVS = 3;NCS = -2.94;LDES = 2.76;DREkm = NA}
  else if(species == 'Amelanchier spp.'){S_Code = 'AMEL';CMIV = 1526;CHHADhi =-19.3;DFS =-0.38;BFS = 0.98;EVS = 2.61;NCS = 1.55;LDES = 2.69;DREkm = NA}
  else if(species == 'Asimina triloba'){S_Code = 'ASTR';CMIV = 552;CHHADhi =-21.4;DFS =-0.48;BFS = -0.32;EVS = 1.42;NCS = -2.37;LDES = -2.65;DREkm = 792}
  else if(species == 'Betula alleghaniensis'){S_Code = 'BEAL';CMIV = 4067;CHHADhi =-68.7;DFS =-1.38;BFS = -0.03;EVS = 1.74;NCS = 2.56;LDES = 2.93;DREkm = NA}
  else if(species == 'Betula lenta'){S_Code = 'BELE';CMIV = 3632;CHHADhi =-42.4;DFS =-1.28;BFS = -0.32;EVS = 0.71;NCS = 2.62;LDES = 2.42;DREkm = 152.28}
  else if(species == 'Betula nigra'){S_Code = 'BENI';CMIV = 788;CHHADhi =290.6;DFS =-0.45;BFS = -0.32;EVS = 0.71;NCS = -0.98;LDES = 0.53;DREkm = 222.2}
  else if(species == 'Betula papyrifera'){S_Code = 'BEPA';CMIV = 6703;CHHADhi =-88.6;DFS =-1.72;BFS = 0.18;EVS = 0.95;NCS = -2.05;LDES = 2.98;DREkm = NA}
  else if(species == 'Betula populifolla'){S_Code = 'BEPO';CMIV = 751;CHHADhi =-32.8;DFS =-1.14;BFS = 0.03;EVS = 2.21;NCS = -1.23;LDES = 2.56;DREkm = NA}
  else if(species == 'Bumelia lanuginose'){S_Code = 'BULA';CMIV = 50;CHHADhi =166;DFS =1.97;BFS = -0.4;EVS = 1.74;NCS = -2.56;LDES = -1.59;DREkm = NA}
  else if(species == 'Carpinus caroliniana'){S_Code = 'CACA';CMIV = 6507;CHHADhi =31.2;DFS =0.56;BFS = 0.62;EVS = 1.89;NCS = -0.73;LDES = 2.72;DREkm = 144.22}
  else if(species == 'Carya aquatica'){S_Code = 'CAAQ';CMIV = 1238;CHHADhi =39.3;DFS =0.86;BFS = -2.04;EVS = 2.05;NCS = -2.81;LDES = 2.2;DREkm = NA}
  else if(species == 'Carya cordiformis'){S_Code = 'CACO';CMIV = 4195;CHHADhi =167.3;DFS =2.17;BFS = -0.83;EVS = -0.55;NCS = 0.66;LDES = 0.82;DREkm = 266.8}
  else if(species == 'Carya glabra'){S_Code = 'CAGL';CMIV = 9615;CHHADhi =-3.3;DFS =0.22;BFS = 0.4;EVS = 0.79;NCS = 1.11;LDES = 2.2;DREkm = 253}
  else if(species == 'Carya illinoensis'){S_Code = 'CAIL';CMIV = 825;CHHADhi =538.8;DFS =-1.24;BFS = -1.7;EVS = 0.08;NCS = 0.92;LDES = -1.9;DREkm = 599.6}
  else if(species == 'Carya laciniosa'){S_Code = 'CALA';CMIV = 202;CHHADhi =258.4;DFS =-0.48;BFS = -0.3;EVS = 0.95;NCS = -1.17;LDES = -2.62;DREkm = 1170.4}
  else if(species == 'Carya ovate'){S_Code = 'CAOV';CMIV = 8241;CHHADhi =-4.8;DFS =-0.2;BFS = 0.37;EVS = 0.16;NCS = 1.23;LDES = 2.21;DREkm = 181.1}
  else if(species == 'Carya texana'){S_Code = 'CATE';CMIV = 2930;CHHADhi =435.5;DFS =1.04;BFS = -2.27;EVS = -0.95;NCS = 1.04;LDES = -1.56;DREkm = 621.2}
  else if(species == 'Carya tomentosa'){S_Code = 'CATO';CMIV = 9770;CHHADhi =34.2;DFS =1.69;BFS = -0.28;EVS = 1.26;NCS = 1.04;LDES = 2.11;DREkm = 296.2}
  else if(species == 'Castanea dentate'){S_Code = 'CADE';CMIV = 169;CHHADhi =-20.7;DFS =0.13;BFS = 0.3;EVS = 3;NCS = -2.94;LDES = 1.84;DREkm = NA}
  else if(species == 'Catalpa speciosa'){S_Code = 'CASP';CMIV = 430;CHHADhi =305.8;DFS =0.93;BFS = -1.58;EVS = -1.11;NCS = -1.11;LDES = -2.37;DREkm = 1000}
  else if(species == 'Celtis laevigata'){S_Code = 'CELA';CMIV = 3851;CHHADhi =274.2;DFS =-0.17;BFS = 0.64;EVS = -0.63;NCS = 0.54;LDES = -1;DREkm = 626.4}
  else if(species == 'Celtis occidentalis'){S_Code = 'CEOC';CMIV = 13010;CHHADhi =50.7;DFS =1.66;BFS = 0.3;EVS = -1.42;NCS = 1.11;LDES = 1.65;DREkm = 189.74}
  else if(species == 'Cercis Canadensis'){S_Code = 'CECA';CMIV = 3037;CHHADhi =70;DFS =0.9;BFS = -0.03;EVS = 0.24;NCS = 1.55;LDES = -0.68;DREkm = 456}
  else if(species == 'Cornus florida'){S_Code = 'COFL';CMIV = 14892;CHHADhi =4.7;DFS =0.07;BFS = 0.95;EVS = 1.11;NCS = -0.16;LDES = 2.19;DREkm = 312.4}
  else if(species == 'Diospyros virginiana'){S_Code = 'DIVI';CMIV = 4194;CHHADhi =199.3;DFS =1.18;BFS = 0.95;EVS = 0;NCS = 0.35;LDES = 1.41;DREkm = 424.2}
  else if(species == 'Fagus grandifolia'){S_Code = 'FAGR';CMIV = 12659;CHHADhi =-52.5;DFS =-1.14;BFS = 0.03;EVS = 1.34;NCS = 1.67;LDES = 2.78;DREkm = 141.42}
  else if(species == 'Fraxinus Americana'){S_Code = 'FRAM';CMIV = 18408;CHHADhi =-28.8;DFS =-2.01;BFS = -0.54;EVS = 0.39;NCS = 1.55;LDES = 2.46;DREkm = 178.88}
  else if(species == 'Fraxinus nigra'){S_Code = 'FRNI';CMIV = 4605;CHHADhi =-53.9;DFS =-1.31;BFS = -3;EVS = 1.18;NCS = -2.68;LDES = 2.64;DREkm = 269.4}
  else if(species == 'Fraxinus pennsylvanica'){S_Code = 'FRPE';CMIV = 20012;CHHADhi =32.5;DFS =-0.13;BFS = -0.25;EVS = -0.63;NCS = -0.16;LDES = 2.5;DREkm = 144.22}
  else if(species == 'Fraxinus quadrangulata'){S_Code = 'FRQU';CMIV = 63;CHHADhi =57.1;DFS =-0.41;BFS = -2.35;EVS = 2.84;NCS = -3;LDES = -0.14;DREkm = NA}
  else if(species == 'Gleditsia aquatic'){S_Code = 'GLAQ';CMIV = 94;CHHADhi =758.5;DFS =0.03;BFS = -0.64;EVS = -0.24;NCS = -0.22;LDES = -3;DREkm = 1109.2}
  else if(species == 'Gleditsia triacanthos'){S_Code = 'GLTR';CMIV = 5804;CHHADhi =147.9;DFS =1.91;BFS = -0.54;EVS = -1.11;NCS = 1.48;LDES = 0.43;DREkm = 288.4}
  else if(species == 'Gordonia lasianthus'){S_Code = 'GOLA';CMIV = 724;CHHADhi =-1;DFS =0.56;BFS = -1.4;EVS = 2.21;NCS = -2.94;LDES = 2.93;DREkm = NA}
  else if(species == 'Gymnocladus dioicus'){S_Code = 'GYDI';CMIV = 336;CHHADhi =185.7;DFS =0.9;BFS = -1.21;EVS = 1.58;NCS = 0.09;LDES = -1.27;DREkm = 475.4}
  else if(species == 'Halesia spp.'){S_Code = 'HALE';CMIV = 60;CHHADhi =31.7;DFS =0.35;BFS = -0.47;EVS = 2.92;NCS = -2.94;LDES = 2.28;DREkm = NA}
  else if(species == 'Ilex opaca'){S_Code = 'ILOP';CMIV = 2390;CHHADhi =1.5;DFS =-0.1;BFS = 0.47;EVS = 2.21;NCS = -1.61;LDES = 2.55;DREkm = 161.2}
  else if(species == 'Juglans cinerea'){S_Code = 'JUCI';CMIV = 318;CHHADhi =-95.9;DFS =-1.41;BFS = -1.27;EVS = 1.82;NCS = -1.55;LDES = 0.55;DREkm = NA}
  else if(species == 'Juglans nigra'){S_Code = 'JUNI';CMIV = 8664;CHHADhi =-18.6;DFS =0.35;BFS = -0.83;EVS = -0.55;NCS = 1.93;LDES = 1.02;DREkm = 228}
  else if(species == 'Liquidambar styraciflua'){S_Code = 'LIST';CMIV = 28185;CHHADhi =23.4;DFS =-0.41;BFS = 0.18;EVS = -1.03;NCS = -0.79;LDES = 1.78;DREkm = 275}
  else if(species == 'Liriodendron tulipifera'){S_Code = 'LITU';CMIV = 12919;CHHADhi =-41.6;DFS =0.13;BFS = 1.26;EVS = 0.63;NCS = -0.03;LDES = 1.98;DREkm = 266.8}
  else if(species == 'Maclura pomifera'){S_Code = 'MAPO';CMIV = 5626;CHHADhi =92.6;DFS =2.32;BFS = 0.33;EVS = -1.18;NCS = 1.61;LDES = -0.02;DREkm = 288.4}
  else if(species == 'Magnolia acuminate'){S_Code = 'MAAC';CMIV = 480;CHHADhi =-27.9;DFS =0.03;BFS = -1.05;EVS = 2.53;NCS = 0.22;LDES = 2.58;DREkm = NA}
  else if(species == 'Magnolia grandiflora'){S_Code = 'MAGR';CMIV = 492;CHHADhi =63.8;DFS =0.56;BFS = 0.4;EVS = 2.45;NCS = -2.37;LDES = 1.65;DREkm = 178.88}
  else if(species == 'Magnolia virginiana'){S_Code = 'MAVI';CMIV = 3318;CHHADhi =43.2;DFS =1.39;BFS = -0.47;EVS = 0.39;NCS = -2.49;LDES = 2.51;DREkm = 141.4}
  else if(species == 'Magnolia macrophylla'){S_Code = 'MAMA';CMIV = 26;CHHADhi =107.7;DFS =0.8;BFS = -0.75;EVS = 2.45;NCS = -3;LDES = -1.12;DREkm = NA}
  else if(species == 'Morus rubra'){S_Code = 'MORU';CMIV = 4689;CHHADhi =258.2;DFS =0.07;BFS = 0.59;EVS = -1.11;NCS = 1.55;LDES = 0.4;DREkm = 250.6}
  else if(species == 'Nyssa aquatica'){S_Code = 'NYAQ';CMIV = 1811;CHHADhi =8.1;DFS =-0.86;BFS = -2.05;EVS = 1.89;NCS = -2.31;LDES = 2.22;DREkm = 466.4}
  else if(species == 'Nyssa ogechee'){S_Code = 'NYOG';CMIV = 79;CHHADhi =-13.9;DFS =-0.51;BFS = -1.77;EVS = 2.37;NCS = -3;LDES = 3;DREkm = NA}
  else if(species == 'Nyssa sylvatica'){S_Code = 'NYSY';CMIV = 10796;CHHADhi =44.2;DFS =1.46;BFS = 0.83;EVS = 0.47;NCS = -0.73;LDES = 2.29;DREkm = 302.6}
  else if(species == 'Nyssa sylvatica B'){S_Code = 'NYSB';CMIV = 5305;CHHADhi =1.7;DFS =-0.65;BFS = -1.7;EVS = 1.26;NCS = -2.49;LDES = 1.96;DREkm = 179.98}
  else if(species == 'Ostrya virginiana'){S_Code = 'OSVI';CMIV = 9598;CHHADhi =25.1;DFS =1.72;BFS = 1.29;EVS = 0.16;NCS = 0.73;LDES = 2.87;DREkm = 134.12}
  else if(species == 'Oxydendrum arboretum'){S_Code = 'OXAR';CMIV = 4278;CHHADhi =-37.8;DFS =2.59;BFS = 0.98;EVS = 1.5;NCS = -1.23;LDES = 2.22;DREkm = 372}
  else if(species == 'Persea borbonia'){S_Code = 'PEBO';CMIV = 1177;CHHADhi =38.1;DFS =2.59;BFS = -0.1;EVS = 2.37;NCS = -2.56;LDES = 2.47;DREkm = 107.68}
  else if(species == 'Planera aquatic'){S_Code = 'PLAQ';CMIV = 411;CHHADhi =50.1;DFS =0.13;BFS = -0.83;EVS = 1.5;NCS = -2.87;LDES = 2.35;DREkm = NA}
  else if(species == 'Platanus occidentalis'){S_Code = 'PLOC';CMIV = 4035;CHHADhi =74.4;DFS =1.28;BFS = -0.9;EVS = 1.18;NCS = 1.36;LDES = 1.47;DREkm = 272}
  else if(species == 'Populus balsamifera'){S_Code = 'POBA';CMIV = 2273;CHHADhi =-48.1;DFS =0.13;BFS = -0.59;EVS = 0.47;NCS = 0.41;LDES = 2.2;DREkm = 145.6}
  else if(species == 'Populus deltoids'){S_Code = 'PODE';CMIV = 9346;CHHADhi =116.4;DFS =0.22;BFS = -0.75;EVS = -1.18;NCS = -0.09;LDES = 2.06;DREkm = 188.68}
  else if(species == 'Populus grandidentata'){S_Code = 'POGR';CMIV = 3959;CHHADhi =-84.7;DFS =1.01;BFS = 0.16;EVS = 0.63;NCS = -1.55;LDES = 2.99;DREkm = NA}
  else if(species == 'Populus tremuloide'){S_Code = 'POTR';CMIV = 18067;CHHADhi =-84.4;DFS =0.56;BFS = 0.01;EVS = 0.87;NCS = -2.56;LDES = 2.94;DREkm = 260}
  else if(species == 'Prunus pensylvanica'){S_Code = 'PRPE';CMIV = 792;CHHADhi =-84.7;DFS =0.45;BFS = -0.68;EVS = 1.82;NCS = 0.47;LDES = 2.61;DREkm = NA}
  else if(species == 'Prunus serotina'){S_Code = 'PRSE';CMIV = 21157;CHHADhi =-46.7;DFS =-1.56;BFS = -0.32;EVS = 0.39;NCS = 0.35;LDES = 2.98;DREkm = 113.14}
  else if(species == 'Prunus virginiana'){S_Code = 'PRVA';CMIV = 1267;CHHADhi =-45.1;DFS =0.18;BFS = -0.86;EVS = 1.5;NCS = -2.68;LDES = 2.24;DREkm = 124.06}
  else if(species == 'Prunus americana'){S_Code = 'PRAM';CMIV = 310;CHHADhi =646.8;DFS =0.48;BFS = -1.34;EVS = -0.24;NCS = -0.35;LDES = -1.74;DREkm = 368.8}
  else if(species == 'Quercus alba'){S_Code = 'QUAL';CMIV = 28284;CHHADhi =-11.9;DFS =1.66;BFS = 1;EVS = -0.32;NCS = 1.04;LDES = 2.78;DREkm = 163.08}
  else if(species == 'Quercus bicolor'){S_Code = 'QUBI';CMIV = 1000;CHHADhi =-28;DFS =1.04;BFS = -0.3;EVS = 0.87;NCS = 1.29;LDES = -0.82;DREkm = 237}
  else if(species == 'Quercus coccinea'){S_Code = 'QUCO';CMIV = 4565;CHHADhi =-37.5;DFS =-0.35;BFS = 0.71;EVS = 0.39;NCS = 1.86;LDES = 1.35;DREkm = 551.8}
  else if(species == 'Quercus durandii'){S_Code = 'QUDU';CMIV = 6;CHHADhi =-33.3;DFS =0;BFS = 0;EVS = 3;NCS = -3;LDES = 1.35;DREkm = NA}
  else if(species == 'Quercus ellipsoidalis'){S_Code = 'QUEL';CMIV = 1178;CHHADhi =14.6;DFS =2.52;BFS = -0.56;EVS = 1.18;NCS = -2.62;LDES = 0.49;DREkm = 305.2}
  else if(species == 'Quercus falcata '){S_Code = 'QUFF';CMIV = 7348;CHHADhi =95.6;DFS =1.21;BFS = 0.21;EVS = -0.16;NCS = -0.92;LDES = 1.48;DREkm = 386.2}
  else if(species == 'Quercus pagoda'){S_Code = 'QUFP';CMIV = 2021;CHHADhi =59.6;DFS =-0.51;BFS = 0.06;EVS = 1.74;NCS = -1.99;LDES = 2.07;DREkm = 160}
  else if(species == 'Quercus ilicifolia'){S_Code = 'QUIL';CMIV = 155;CHHADhi =49;DFS =1.04;BFS = -0.81;EVS = 2.84;NCS = 0.03;LDES = 2.13;DREkm = 82.46}
  else if(species == 'Quercus imbricaria'){S_Code = 'QUIM';CMIV = 1779;CHHADhi =53.7;DFS =1.31;BFS = -0.74;EVS = 0.63;NCS = -0.03;LDES = -0.54;DREkm = 565.6}
  else if(species == 'Quercus laevis'){S_Code = 'QULA';CMIV = 1398;CHHADhi =49.5;DFS =2.62;BFS = -0.88;EVS = 1.34;NCS = -2.68;LDES = 2.38;DREkm = 101.96}
  else if(species == 'Quercus laurifolia'){S_Code = 'QULA2';CMIV = 5168;CHHADhi =29.8;DFS =0.18;BFS = 0.13;EVS = 0.32;NCS = -2.56;LDES = 1.55;DREkm = 220}
  else if(species == 'Quercus lyrata'){S_Code = 'QULY';CMIV = 1694;CHHADhi =60;DFS =-0.51;BFS = -0.95;EVS = 0.71;NCS = -2.24;LDES = 1.81;DREkm = 244.2}
  else if(species == 'Quercus macrocarpa'){S_Code = 'QYMA';CMIV = 12197;CHHADhi =63.9;DFS =2.77;BFS = -0.16;EVS = -1.74;NCS = 0.85;LDES = 1.13;DREkm = 320}
  else if(species == 'Quercus marilandica'){S_Code = 'QUMA2';CMIV = 3453;CHHADhi =398.6;DFS =1.56;BFS = 0.21;EVS = -0.55;NCS = 1.55;LDES = -0.48;DREkm = 627.6}
  else if(species == 'Quercus michauxii'){S_Code = 'QUMI';CMIV = 713;CHHADhi =-6.6;DFS =1.08;BFS = -0.81;EVS = 2.37;NCS = -2.43;LDES = 1.78;DREkm = 241.6}
  else if(species == 'Quercus muehlenbergii'){S_Code = 'QUMU';CMIV = 2313;CHHADhi =81.4;DFS =1.18;BFS = -0.66;EVS = 0.39;NCS = 2.05;LDES = -0.85;DREkm = 462.8}
  else if(species == 'Quercus nigra'){S_Code = 'QUNI';CMIV = 11570;CHHADhi =82.5;DFS =-0.17;BFS = -0.59;EVS = -0.63;NCS = -0.79;LDES = 0.26;DREkm = 546.2}
  else if(species == 'Quercus nuttallii'){S_Code = 'QUNU';CMIV = 633;CHHADhi =84.4;DFS =2.83;BFS = -0.09;EVS = 0.63;NCS = -1.55;LDES = 0.52;DREkm = 561.4}
  else if(species == 'Quercus palustris'){S_Code = 'QUPA';CMIV = 1927;CHHADhi =99.5;DFS =-0.65;BFS = -1.39;EVS = -0.08;NCS = -0.66;LDES = -0.05;DREkm = 286.4}
  else if(species == 'Quercus phellos'){S_Code = 'QUPH';CMIV = 3591;CHHADhi =70.8;DFS =0.63;BFS = -0.01;EVS = 1.11;NCS = -1.55;LDES = 1.51;DREkm = 240.8}
  else if(species == 'Quercus prinus'){S_Code = 'QUPR';CMIV = 7933;CHHADhi =-27.1;DFS =1.39;BFS = 1.29;EVS = 0.47;NCS = 1.17;LDES = 2;DREkm = 466.8}
  else if(species == 'Quercus rubra'){S_Code = 'QURU';CMIV = 18801;CHHADhi =-39.6;DFS =1.39;BFS = 0.13;EVS = -0.63;NCS = 1.67;LDES = 2.85;DREkm = 179.44}
  else if(species == 'Quercus shumardii'){S_Code = 'QUSH';CMIV = 233;CHHADhi =1502.1;DFS =2.45;BFS = -1.02;EVS = 0.55;NCS = -0.09;LDES = -2.84;DREkm = 588.6}
  else if(species == 'Quercus stellata'){S_Code = 'QUST';CMIV = 14630;CHHADhi =320.2;DFS =2.17;BFS = -0.59;EVS = -1.26;NCS = 1.23;LDES = 0.87;DREkm = 480.8}
  else if(species == 'Quercus velutina'){S_Code = 'QUVE';CMIV = 16081;CHHADhi =25.7;DFS =0.51;BFS = 0.42;EVS = 0.32;NCS = 2.49;LDES = 2.73;DREkm = 159.98}
  else if(species == 'Quercus virginiana'){S_Code = 'QUVI';CMIV = 2790;CHHADhi =213.2;DFS =0.93;BFS = 0.03;EVS = -2.29;NCS = -3;LDES = -0.79;DREkm = 460.4}
  else if(species == 'Quercus incana'){S_Code = 'QUIN';CMIV = 501;CHHADhi =116.4;DFS =0.66;BFS = 0.03;EVS = 1.11;NCS = -2.75;LDES = 2.11;DREkm = 116.62}
  else if(species == 'Robinia pseudoacacia'){S_Code = 'ROPS';CMIV = 4685;CHHADhi =101.1;DFS =0;BFS = -0.59;EVS = -1.26;NCS = 1.17;LDES = 0.36;DREkm = 291.2}
  else if(species == 'Salix amygdaloides'){S_Code = 'SAAM';CMIV = 255;CHHADhi =714.5;DFS =0.13;BFS = -1.65;EVS = -1.11;NCS = -0.54;LDES = 0.67;DREkm = 247.4}
  else if(species == 'Salix nigra'){S_Code = 'SANI';CMIV = 4866;CHHADhi =134;DFS =-0.31;BFS = -2.13;EVS = -0.39;NCS = 0.85;LDES = 1.65;DREkm = 200}
  else if(species == 'Sassafras albidum'){S_Code = 'SASAL';CMIV = 7563;CHHADhi =6.4;DFS =0.48;BFS = -0.64;EVS = 0.71;NCS = 0.28;LDES = 1.96;DREkm = 280}
  else if(species == 'Sorbus americana'){S_Code = 'SOAM';CMIV = 35;CHHADhi =-91.4;DFS =-0.23;BFS = -1.62;EVS = 2.76;NCS = -3;LDES = 0.76;DREkm = NA}
  else if(species == 'Tilia americana'){S_Code = 'TIAM';CMIV = 8479;CHHADhi =-22;DFS =0.31;BFS = 0.16;EVS = 0.47;NCS = -2.43;LDES = 2.62;DREkm = 178.88}
  else if(species == 'Ulmus alata'){S_Code = 'ULAL';CMIV = 7378;CHHADhi =370.9;DFS =-0.58;BFS = -0.3;EVS = -1.18;NCS = 0.92;LDES = -0.26;DREkm = 544}
  else if(species == 'Ulmus americana'){S_Code = 'ULAM';CMIV = 28934;CHHADhi =4.5;DFS =-0.8;BFS = 0.3;EVS = -2.05;NCS = 1.04;LDES = 2.58;DREkm = 152.28}
  else if(species == 'Ulmus crassifolia'){S_Code = 'ULCR';CMIV = 544;CHHADhi =926.7;DFS =-0.27;BFS = -1.17;EVS = -0.87;NCS = 1.04;LDES = -2.46;DREkm = 823.4}
  else if(species == 'Ulmus rubra'){S_Code = 'ULRU';CMIV = 8758;CHHADhi =12.2;DFS =0.03;BFS = 0.68;EVS = 0.79;NCS = 1.17;LDES = 2.13;DREkm = 198.98}
  else if(species == 'Ulmus thomasii'){S_Code = 'ULTH';CMIV = 326;CHHADhi =6.1;DFS =-0.2;BFS = -2.61;EVS = 1.18;NCS = -0.28;LDES = -0.55;DREkm = 237.4}
  else{S_Code = 'NA';CMIV = NA;CHHADhi =NA;DFS =NA;BFS =NA;EVS = NA;NCS = NA;LDES = NA;DREkm =NA} 
  
  all_info = c(S_Code = S_Code,CMIV = CMIV,CHHADhi =CHHADhi,DFS =DFS,BFS =BFS,EVS = EVS,NCS = NCS,LDES = LDES,DREkm =DREkm)
  
  return(all_info)
}

