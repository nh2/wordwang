story1 = ["2003, I pray for God they people make the right decision
I don't wanna war. I just wanna peace. Stop the war. Check this.
I hope my black brothers feel the same like me
Dre, Snoop, Puff, L, Tupac Shakur, rest in peace. He was the best. My respect (yeee, c'mon)",

"I hate terrorists, and I understand you.
September 11, I'll never forget you. Rest in peace
Catch the bad man, stop your plan, bin Laden, thank Allah.
Yee c'mon. Stop the war. That's right.",

"Sometimes people make a war, don't know what is for (business)
Say you stop the war (yee c'mon, once again)
Sometimes people fight a war, don't know what is for (business)
Say you stop the war (yee that's right, c'mon)",

"I don't wanna war, I just want to live and love each other
My family, my friends. Nobody wants war. Life is short
Yee, c'mon, that's right. Check.",

"2003, I pray for God they people make the right decision
I don't wanna war. I just wanna peace. Stop the war. Check this.
I hope my black brothers feel the same like me
Dre, Snoop, Puff, L, Tupac Shakur, rest in peace. He was the best. My respect (yeee, c'mon)",

"I hate terrorists, and I understand you.
September 11, I'll never forget you. Rest in peace
Catch the bad man, stop your plan, bin Laden, thank Allah.
Yee c'mon. Stop the war. That's right.",

"Sometimes people make a war, don't know what is for (business)
Say you stop the war (yee c'mon, once again)
Sometimes people fight a war, don't know what is for (business)
Say you stop the war (yee that's right, c'mon)",

"I don't wanna war, I just want to live and love each other
My family, my friends. Nobody wants war. Life is short
Yee, c'mon, that's right. Check."]

window.fixture1 = ->
  do ->
    new @suggestion('once', 1).add()
    new @suggestion('there', 3).add()
    new @suggestion('was', 2).add()
    new @suggestion('a', 7).add()
    new @suggestion('an', 5).add()
    new @suggestion('old', 10).add()
    new @suggestion('man', 6).add()
  window.ui.story story1
