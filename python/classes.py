from __future__ import print_function

class Animal:
	
	def __init__(self, name, sound):
		self.name = name
		self.sound = sound
				
	def talk(self):
		print("%s: %s" % (self.name, self.sound))
		
class Tiger(Animal):
	def __init__(self):
		Animal.__init__(self, "Tiger", "Roar!!!")
		
class Horse(Animal):
	def __init__(self):
		Animal.__init__(self, "Horse", "Whinny.")


if __name__ == "__main__":
	t = Tiger()
	t.talk()
	
	h = Horse()
	h.talk()

