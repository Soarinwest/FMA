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
#
# 
# Inputs: 
# 
#

ADAPT = function(species,desired_return){
  if(is.na(species)){name = 'Tree unknown';Score = NA} #unknown/ na action first to avoid missing value where true/false needed 
  else if(species == 'Abies balsamea'){name = 'balsam fir'; Score = 2.7} 	
  else if(species == 'Chamaecyparis thyoides'){name = 'Atlantic white-cedar'; Score = 3}
  else if(species == 'Juniperus ashei'){name = 'ashe juniper'; Score = NA}
  else if(species == 'Juniperus virginiana'){name = 'eastern redcedar'; Score = 3.9}
  else if(species == 'Larix laricina'){name = 'tamarack (native)'; Score = 3.1}
  else if(species == 'Picea abies'){name = 'Norway spruce'; Score = NA}
  else if(species == 'Picea glauca'){name = 'white spruce'; Score = 3.9}
  else if(species == 'Picea mariana'){name = 'black spruce'; Score = 4.3}
  else if(species == 'Picea rubens'){name = 'red spruce'; Score = 2.9}
  else if(species == 'Pinus banksiana'){name = 'jack pine'; Score = 5.2}
  else if(species == 'Pinus clausa'){name = 'sand pine'; Score = 2.7}
  else if(species == 'Pinus echinata'){name = 'shortleaf pine'; Score = 3.6}
  else if(species == 'Pinus elliottii'){name = 'slash pine'; Score = 4.3}
  else if(species == 'Pinus glabra'){name = 'spruce pine'; Score = 4.4}
  else if(species == 'Pinus palustris'){name = 'longleaf pine'; Score = 4.2}
  else if(species == 'Pinus pungens'){name = 'Table Mountain pine'; Score = 5.9}
  else if(species == 'Pinus resinosa'){name = 'red pine'; Score = 3}
  else if(species == 'Pinus rigida'){name = 'pitch pine'; Score = 3.8}
  else if(species == 'Pinus serotina'){name = 'pond pine'; Score = 2.4}
  else if(species == 'Pinus strobus'){name = 'eastern white pine'; Score = 3.3}
  else if(species == 'Pinus sylvestris'){name = 'Scots pine'; Score = NA}
  else if(species == 'Pinus taeda'){name = 'loblolly pine'; Score = 3.4}
  else if(species == 'Pinus virginiana'){name = 'Virginia pine'; Score = 3.8}
  else if(species == 'Taxodium distichum'){name = 'bald cypress'; Score = 3.9}
  else if(species == 'Taxodium ascendens'){name = 'pond cypress'; Score = 5}
  else if(species == 'Thuja occidentalis'){name = 'northern white-cedar'; Score = 4.2}
  else if(species == 'Tsuga canadensis'){name = 'eastern hemlock'; Score = 2.7}
  else if(species == 'Acer barbatum'){name = 'florida maple'; Score = 6.1}
  else if(species == 'Acer negundo'){name = 'boxelder'; Score = 7.4}
  else if(species == 'Acer nigrum'){name = 'black maple'; Score = 5.2}
  else if(species == 'Acer pensylvanicum'){name = 'striped maple'; Score = 5.1}
  else if(species == 'Acer rubrum'){name = 'red maple'; Score = 8.5}
  else if(species == 'Acer saccharinum'){name = 'silver maple'; Score = 5.6}
  else if(species == 'Acer saccharum'){name = 'sugar maple'; Score = 5.8}
  else if(species == 'Acer spicatum'){name = 'mountain maple'; Score = 5.9}
  else if(species == 'Acer platanoides'){name = 'Norway maple'; Score = NA}
  else if(species == 'Aesculus glabra'){name = 'Ohio buckeye'; Score = 3.5}
  else if(species == 'Aesculus flava'){name = 'yellow buckeye'; Score = 3.1}
  else if(species == 'Ailanthus altissima'){name = 'ailanthus'; Score = NA}
  else if(species == 'Amelanchier spp.'){name = 'serviceberry'; Score = 4.8}
  else if(species == 'Asimina triloba'){name = 'pawpaw'; Score = 3.7}
  else if(species == 'Betula alleghaniensis'){name = 'yellow birch'; Score = 3.4}
  else if(species == 'Betula lenta'){name = 'sweet birch'; Score = 3.2}
  else if(species == 'Betula nigra'){name = 'river birch'; Score = 3.7}
  else if(species == 'Betula papyrifera'){name = 'paper birch'; Score = 3.4}
  else if(species == 'Betula populifolia'){name = 'gray birch'; Score = 3.6}
  else if(species == 'Sideroxylon lanuginosum ssp. lanuginosum'){name = 'cittamwood/gum bumelia'; Score = 5.6}
  else if(species == 'Carpinus caroliniana'){name = 'American hornbeam; musclewood'; Score = 5.1}
  else if(species == 'Carya aquatica'){name = 'water hickory'; Score = 4}
  else if(species == 'Carya cordiformis'){name = 'bitternut hickory'; Score = 5.6}
  else if(species == 'Carya glabra'){name = 'pignut hickory'; Score = 4.7}
  else if(species == 'Carya illinoinensis'){name = 'pecan'; Score = 2.2}
  else if(species == 'Carya laciniosa'){name = 'shellbark hickory'; Score = 3.7}
  else if(species == 'Carya ovata'){name = 'shagbark hickory'; Score = 4.4}
  else if(species == 'Carya texana'){name = 'black hickory'; Score = 4.1}
  else if(species == 'Carya alba'){name = 'mockernut hickory'; Score = 5.4}
  else if(species == 'Carya pallida'){name = 'sand hickory'; Score = NA}
  else if(species == 'Castanea dentata'){name = 'American chestnut'; Score = 4.5}
  else if(species == 'Catalpa speciosa'){name = 'northern catalpa'; Score = 4.2}
  else if(species == 'Celtis laevigata'){name = 'sugarberry'; Score = 4.6}
  else if(species == 'Celtis occidentalis'){name = 'hackberry'; Score = 5.7}
  else if(species == 'Cercis canadensis'){name = 'eastern redbud'; Score = 4.9}
  else if(species == 'Cornus florida'){name = 'flowering dogwood'; Score = 5}
  else if(species == 'Diospyros virginiana'){name = 'common persimmon'; Score = 5.8}
  else if(species == 'Fagus grandifolia'){name = 'American beech'; Score = 3.6}
  else if(species == 'Fraxinus americana'){name = 'white ash'; Score = 2.7}
  else if(species == 'Fraxinus nigra'){name = 'black ash'; Score = 1.7}
  else if(species == 'Fraxinus pennsylvanica'){name = 'green ash'; Score = 4}
  else if(species == 'Fraxinus profunda'){name = 'pumpkin ash'; Score = NA}
  else if(species == 'Fraxinus quadrangulata'){name = 'blue ash'; Score = 2.7}
  else if(species == 'Fraxinus caroliniana'){name = 'Carolina ash'; Score = NA}
  else if(species == 'Fraxinus texensis'){name = 'Texas ash'; Score = NA}
  else if(species == 'Gleditsia aquatica'){name = 'waterlocust'; Score = 3.8}
  else if(species == 'Gleditsia triacanthos'){name = 'honeylocust'; Score = 5.5}
  else if(species == 'Gordonia lasianthus'){name = 'loblolly-bay'; Score = 3.9}
  else if(species == 'Gymnocladus dioicus'){name = 'Kentucky coffeetree'; Score = 4.3}
  else if(species == 'Halesia spp.'){name = 'silverbell'; Score = 4.2}
  else if(species == 'Ilex opaca'){name = 'American holly'; Score = 4.5}
  else if(species == 'Juglans cinerea'){name = 'butternut'; Score = 2.3}
  else if(species == 'Juglans nigra'){name = 'black walnut'; Score = 4}
  else if(species == 'Liquidambar styraciflua'){name = 'sweetgum'; Score = 4.1}
  else if(species == 'Liriodendron tulipifera'){name = 'yellow-poplar'; Score = 5.3}
  else if(species == 'Maclura pomifera'){name = 'Osage-orange'; Score = 6.3}
  else if(species == 'Magnolia acuminata'){name = 'cucumbertree'; Score = 3.6}
  else if(species == 'Magnolia grandiflora'){name = 'southern magnolia'; Score = 4.9}
  else if(species == 'Magnolia virginiana'){name = 'sweetbay'; Score = 5.1}
  else if(species == 'Magnolia macrophylla'){name = 'bigleaf magnolia'; Score = 4.4}
  else if(species == 'Magnolia fraseri'){name = 'mountain or Fraser magnolia'; Score = NA}
  else if(species == 'Morus alba'){name = 'white mulberry'; Score = NA}
  else if(species == 'Morus rubra'){name = 'red mulberry'; Score = 4.7}
  else if(species == 'Nyssa aquatica'){name = 'water tupelo'; Score = 2.3}
  else if(species == 'Nyssa ogeche'){name = 'ogeechee tupelo'; Score = 2.8}
  else if(species == 'Nyssa sylvatica'){name = 'blackgum'; Score = 5.9}
  else if(species == 'Nyssa biflora'){name = 'swamp tupelo'; Score = 2.7}
  else if(species == 'Ostrya virginiana'){name = 'eastern hophornbeam; ironwood'; Score = 6.4}
  else if(species == 'Oxydendrum arboreum'){name = 'sourwood'; Score = 6.9}
  else if(species == 'Paulownia tomentosa'){name = 'paulownia'; Score = NA}
  else if(species == 'Persea borbonia'){name = 'redbay'; Score = 6.3}
  else if(species == 'Planera aquatica'){name = 'water elm'; Score = 3.8}
  else if(species == 'Platanus occidentalis'){name = 'sycamore'; Score = 4.8}
  else if(species == 'Populus balsamifera'){name = 'balsam poplar'; Score = 4}
  else if(species == 'Populus deltoides'){name = 'eastern cottonwood'; Score = 3.9}
  else if(species == 'Populus grandidentata'){name = 'bigtooth aspen'; Score = 5.1}
  else if(species == 'Populus tremuloides'){name = 'quaking aspen'; Score = 4.7}
  else if(species == 'Prunus pensylvanica'){name = 'pin cherry'; Score = 4.2}
  else if(species == 'Prunus serotina'){name = 'black cherry'; Score = 3}
  else if(species == 'Prunus virginiana'){name = 'chokecherry'; Score = 3.8}
  else if(species == 'Prunus americana'){name = 'wild plum'; Score = 3.9}
  else if(species == 'Quercus alba'){name = 'white oak'; Score = 6.1}
  else if(species == 'Quercus bicolor'){name = 'swamp white oak'; Score = 4.9}
  else if(species == 'Quercus coccinea'){name = 'scarlet oak'; Score = 4.6}
  else if(species == 'Quercus sinuata var. sinuata'){name = 'durand oak'; Score = 4.2}
  else if(species == 'Quercus ellipsoidalis'){name = 'northern pin oak'; Score = 6}
  else if(species == 'Quercus falcata'){name = 'southern red oak'; Score = 5.3}
  else if(species == 'Quercus pagoda'){name = 'cherrybark oak; swamp red oak'; Score = 3.9}
  else if(species == 'Quercus ilicifolia'){name = 'bear oak; scrub oak'; Score = 4.6}
  else if(species == 'Quercus imbricaria'){name = 'shingle oak'; Score = 4.9}
  else if(species == 'Quercus laevis'){name = 'turkey oak'; Score = 6}
  else if(species == 'Quercus laurifolia'){name = 'laurel oak'; Score = 4.5}
  else if(species == 'Quercus lyrata'){name = 'overcup oak'; Score = 3.2}
  else if(species == 'Quercus macrocarpa'){name = 'bur oak'; Score = 6.4}
  else if(species == 'Quercus marilandica'){name = 'blackjack oak'; Score = 5.6}
  else if(species == 'Quercus michauxii'){name = 'swamp chestnut oak'; Score = 4.6}
  else if(species == 'Quercus muehlenbergii'){name = 'chinkapin oak'; Score = 4.8}
  else if(species == 'Quercus nigra'){name = 'water oak'; Score = 3.7}
  else if(species == 'Quercus texana'){name = 'Nuttall oak'; Score = 6.5}
  else if(species == 'Quercus palustris'){name = 'pin oak'; Score = 2.8}
  else if(species == 'Quercus phellos'){name = 'willow oak'; Score = 4.7}
  else if(species == 'Quercus prinus'){name = 'chestnut oak'; Score = 6.1}
  else if(species == 'Quercus rubra'){name = 'northern red oak'; Score = 5.4}
  else if(species == 'Quercus shumardii'){name = 'Shumard oak'; Score = 5.8}
  else if(species == 'Quercus stellata'){name = 'post oak'; Score = 5.7}
  else if(species == 'Quercus velutina'){name = 'black oak'; Score = 4.9}
  else if(species == 'Quercus virginiana'){name = 'live oak'; Score = 5}
  else if(species == 'Quercus incana'){name = 'bluejack oak'; Score = 4.8}
  else if(species == 'Robinia pseudoacacia'){name = 'black locust'; Score = 3.8}
  else if(species == 'Sabal palmetto'){name = 'cabbage palmetto'; Score = NA}
  else if(species == 'Salix amygdaloides'){name = 'peachleaf willow'; Score = 3.4}
  else if(species == 'Salix nigra'){name = 'black willow'; Score = 2.8}
  else if(species == 'Sassafras albidum'){name = 'sassafras'; Score = 4.2}
  else if(species == 'Sorbus americana'){name = 'American mountain-ash'; Score = 3.1}
  else if(species == 'Tilia americana'){name = 'American basswood'; Score = 4.6}
  else if(species == 'Ulmus alata'){name = 'winged elm'; Score = 3.6}
  else if(species == 'Ulmus americana'){name = 'American elm'; Score = 4}
  else if(species == 'Ulmus crassifolia'){name = 'cedar elm'; Score = 3.3}
  else if(species == 'Ulmus pumila'){name = 'Siberian elm'; Score = NA}
  else if(species == 'Ulmus rubra'){name = 'slippery elm'; Score = 4.8}
  else if(species == 'Ulmus thomasii'){name = 'rock elm'; Score = 2.8}
  else{name = 'Tree unknown';Score = NA} 
  
  all_info = c(name = name,Score=Score)
  
  if(desired_return == "score") return(Score)
  else if(desired_return == "all") return(all_info)
}
