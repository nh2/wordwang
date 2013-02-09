@log = (args...) -> console.log args...
@dir = (obj) -> console.log(JSON.stringify obj)
@assert = (bool, msg) ->
  unless bool
    throw n

loremIpsum = ["2003, I pray for God they people make the right decision
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

class @UI
  constructor: ->

    @story = ko.observableArray loremIpsum
    @suggestion = ko.observable ''
    @suggestions = ko.observableArray []

  suggest: (args...) ->
    new window.suggestion(@suggestion()).add()
    @suggestion ''

connectServer = ->

main = ->
  window.ui = new UI()
  ko.applyBindings(ui)

  #connectServer()


$ ->
  # Unfortunately not all browsers have window.location.origin
  origin = window.location.protocol + '//' + window.location.host

  main()
