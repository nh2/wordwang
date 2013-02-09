story1 = ["John Stalvern waited. The lights above him blinked and sparked out of the air. There were demons in the base. He didn’t see them, but had expected them now for years. His warnings to Cernel Joson were not listenend to and now it was too late. Far too late for now, anyway.",
"John was a space marine for fourteen years. When he was young he watched the spaceships and he said to dad “I want to be on the ships daddy.”",
"Dad said “No! You will BE KILL BY DEMONS”",
"There was a time when he believed him. Then as he got oldered he stopped. But now in the space station base of the UAC he knew there were demons.",
"“This is Joson” the radio crackered. “You must fight the demons!”",
"So John gotted his palsma rifle and blew up the wall.",
"“HE GOING TO KILL US” said the demons",
"“I will shoot at him” said the cyberdemon and he fired the rocket missiles. John plasmaed at him and tried to blew him up. But then the ceiling fell and they were trapped and not able to kill.",
"“No! I must kill the demons” he shouted",
"The radio said “No, John. You are the demons”",
"And then John was a zombie."]

window.fixture1 = ->
  @ui.addSuggestion('once there was', 1)
  @ui.addSuggestion('there lived an', 3)
  @ui.addSuggestion('was', 2)
  @ui.addSuggestion('an evil witch', 7)
  @ui.addSuggestion('an apple', 5)
  @ui.addSuggestion('old woman', 10)
  @ui.addSuggestion('man, you', 6)
  @ui.addSuggestion('thanks to', 12)
  @ui.addSuggestion('he shouted', 8)
  @ui.addSuggestion('cloudy apple juice', 4)

  window.ui.story story1
  window.ui.username "wookie"
  window.ui.joined true