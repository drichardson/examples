import classes # Defines animals that will be used in our Zoo.

class Zoo:
	def __init__(self):
		self.animals = []
		
	def addAnimal(self, animal):
		self.animals.append(animal)
		

def printAnimals(zoo):
	for x in zoo.animals:
		print "Animal: %s" % x.talk()

zoo = Zoo();
printAnimals(zoo)

zoo.addAnimal(classes.Tiger())
printAnimals(zoo)

zoo.addAnimal(classes.Horse())
printAnimals(zoo)

zoo.addAnimal(classes.Horse())
printAnimals(zoo)
