//array of scientific statements
var scientific_statements = [
  {
    "Category": "Astronomy",
    "Concept": "Planet",
    "T1": "T",
    "T2": "T",
    "Statement": "Planets are more massive than moons.",
    "M proportion correct": 0.83,
    "M response time (ms)": 3943
  },
  {
    "Category": "Astronomy",
    "Concept": "Planet",
    "T1": "F",
    "T2": "F",
    "Statement": "Asteroids are more massive than planets.",
    "M proportion correct": 0.79,
    "M response time (ms)": 3913
  },
  {
    "Category": "Astronomy",
    "Concept": "Planet",
    "T1": "T",
    "T2": "F",
    "Statement": "Planets are more massive than stars.",
    "M proportion correct": 0.64,
    "M response time (ms)": 3129
  },
  {
    "Category": "Astronomy",
    "Concept": "Planet",
    "T1": "F",
    "T2": "T",
    "Statement": "Stars are more massive than planets.",
    "M proportion correct": 0.61,
    "M response time (ms)": 3726
  },
  {
    "Category": "Astronomy",
    "Concept": "Star",
    "T1": "T",
    "T2": "T",
    "Statement": "The sun produces light.",
    "M proportion correct": 0.99,
    "M response time (ms)": 2546
  },
  {
    "Category": "Astronomy",
    "Concept": "Star",
    "T1": "F",
    "T2": "F",
    "Statement": "The sun produces sound.",
    "M proportion correct": 0.84,
    "M response time (ms)": 2906
  },
  {
    "Category": "Astronomy",
    "Concept": "Star",
    "T1": "T",
    "T2": "F",
    "Statement": "The moon produces light.",
    "M proportion correct": 0.65,
    "M response time (ms)": 3111
  },
  {
    "Category": "Astronomy",
    "Concept": "Star",
    "T1": "F",
    "T2": "T",
    "Statement": "The sun produces gravity.",
    "M proportion correct": 0.54,
    "M response time (ms)": 3348
  },
  {
    "Category": "Astronomy",
    "Concept": "Solar system",
    "T1": "T",
    "T2": "T",
    "Statement": "The moon revolves around the earth.",
    "M proportion correct": 0.95,
    "M response time (ms)": 3121
  },
  {
    "Category": "Astronomy",
    "Concept": "Solar system",
    "T1": "F",
    "T2": "F",
    "Statement": "The sun revolves around the moon.",
    "M proportion correct": 0.94,
    "M response time (ms)": 3271
  },
  {
    "Category": "Astronomy",
    "Concept": "Solar system",
    "T1": "T",
    "T2": "F",
    "Statement": "The sun revolves around the earth.",
    "M proportion correct": 0.86,
    "M response time (ms)": 3889
  },
  {
    "Category": "Astronomy",
    "Concept": "Solar system",
    "T1": "F",
    "T2": "T",
    "Statement": "The earth revolves around the sun.",
    "M proportion correct": 0.83,
    "M response time (ms)": 2948
  },
  {
    "Category": "Astronomy",
    "Concept": "Lunar phase",
    "T1": "T",
    "T2": "T",
    "Statement": "Phases of the moon are caused by changes in illumination.",
    "M proportion correct": 0.95,
    "M response time (ms)": 3576
  },
  {
    "Category": "Astronomy",
    "Concept": "Lunar phase",
    "T1": "F",
    "T2": "F",
    "Statement": "Phases of the moon are caused by clouds.",
    "M proportion correct": 0.93,
    "M response time (ms)": 3527
  },
  {
    "Category": "Astronomy",
    "Concept": "Lunar phase",
    "T1": "T",
    "T2": "F",
    "Statement": "Phases of the moon are caused by the earth's shadow.",
    "M proportion correct": 0.39,
    "M response time (ms)": 5285
  },
  {
    "Category": "Astronomy",
    "Concept": "Lunar phase",
    "T1": "F",
    "T2": "T",
    "Statement": "Phases of the moon are caused by the moon's orbit.",
    "M proportion correct": 0.68,
    "M response time (ms)": 4784
  },
  {
    "Category": "Astronomy",
    "Concept": "Season",
    "T1": "T",
    "T2": "T",
    "Statement": "A change in overall sunlight causes the seasons.",
    "M proportion correct": 0.95,
    "M response time (ms)": 3855
  },
  {
    "Category": "Astronomy",
    "Concept": "Season",
    "T1": "F",
    "T2": "F",
    "Statement": "The earth's rotation causes the seasons.",
    "M proportion correct": 0.67,
    "M response time (ms)": 4901
  },
  {
    "Category": "Astronomy",
    "Concept": "Season",
    "T1": "T",
    "T2": "F",
    "Statement": "The earth's distance from the sun causes the seasons.",
    "M proportion correct": 0.49,
    "M response time (ms)": 4457
  },
  {
    "Category": "Astronomy",
    "Concept": "Season",
    "T1": "F",
    "T2": "T",
    "Statement": "The earth's tilt causes the seasons.",
    "M proportion correct": 0.75,
    "M response time (ms)": 3590
  },
  {
    "Category": "Evolution",
    "Concept": "Common ancestry",
    "T1": "T",
    "T2": "T",
    "Statement": "Humans are more closely related to apes than monkeys.",
    "M proportion correct": 0.87,
    "M response time (ms)": 4122
  },
  {
    "Category": "Evolution",
    "Concept": "Common ancestry",
    "T1": "F",
    "T2": "F",
    "Statement": "Whales are more closely related to plants than fish.",
    "M proportion correct": 0.97,
    "M response time (ms)": 4204
  },
  {
    "Category": "Evolution",
    "Concept": "Common ancestry",
    "T1": "T",
    "T2": "F",
    "Statement": "Apes are more closely related to monkeys than humans.",
    "M proportion correct": 0.41,
    "M response time (ms)": 4983
  },
  {
    "Category": "Evolution",
    "Concept": "Common ancestry",
    "T1": "F",
    "T2": "T",
    "Statement": "Whales are more closely related to humans than fish.",
    "M proportion correct": 0.51,
    "M response time (ms)": 4563
  },
  {
    "Category": "Evolution",
    "Concept": "Phylogeny",
    "T1": "T",
    "T2": "T",
    "Statement": "Humans are descended from tree-dwelling creatures.",
    "M proportion correct": 0.51,
    "M response time (ms)": 3557
  },
  {
    "Category": "Evolution",
    "Concept": "Phylogeny",
    "T1": "F",
    "T2": "F",
    "Statement": "Humans are descended from plants.",
    "M proportion correct": 0.89,
    "M response time (ms)": 4089
  },
  {
    "Category": "Evolution",
    "Concept": "Phylogeny",
    "T1": "T",
    "T2": "F",
    "Statement": "Humans are descended from chimpanzees.",
    "M proportion correct": 0.41,
    "M response time (ms)": 3448
  },
  {
    "Category": "Evolution",
    "Concept": "Phylogeny",
    "T1": "F",
    "T2": "T",
    "Statement": "Humans are descended from sea-dwelling creatures.",
    "M proportion correct": 0.55,
    "M response time (ms)": 5035
  },
  {
    "Category": "Evolution",
    "Concept": "Variation",
    "T1": "T",
    "T2": "T",
    "Statement": "Evolution requires differential survival.",
    "M proportion correct": 0.91,
    "M response time (ms)": 4044
  },
  {
    "Category": "Evolution",
    "Concept": "Variation",
    "T1": "F",
    "T2": "F",
    "Statement": "Evolution requires climate stability.",
    "M proportion correct": 0.66,
    "M response time (ms)": 4052
  },
  {
    "Category": "Evolution",
    "Concept": "Variation",
    "T1": "T",
    "T2": "F",
    "Statement": "Evolution requires long periods of time.",
    "M proportion correct": 0.09,
    "M response time (ms)": 4237
  },
  {
    "Category": "Evolution",
    "Concept": "Variation",
    "T1": "F",
    "T2": "T",
    "Statement": "Evolution requires within-species variation.",
    "M proportion correct": 0.66,
    "M response time (ms)": 4128
  },
  {
    "Category": "Evolution",
    "Concept": "Selection",
    "T1": "T",
    "T2": "T",
    "Statement": "Most organisms are adapted to their environment.",
    "M proportion correct": 0.95,
    "M response time (ms)": 4066
  },
  {
    "Category": "Evolution",
    "Concept": "Selection",
    "T1": "F",
    "T2": "F",
    "Statement": "Most organisms live in temperate climates.",
    "M proportion correct": 0.48,
    "M response time (ms)": 3141
  },
  {
    "Category": "Evolution",
    "Concept": "Selection",
    "T1": "T",
    "T2": "F",
    "Statement": "Most organisms have plenty to eat.",
    "M proportion correct": 0.63,
    "M response time (ms)": 3722
  },
  {
    "Category": "Evolution",
    "Concept": "Selection",
    "T1": "F",
    "T2": "T",
    "Statement": "Most organisms die before leaving offspring.",
    "M proportion correct": 0.33,
    "M response time (ms)": 3496
  },
  {
    "Category": "Evolution",
    "Concept": "Adaptation",
    "T1": "T",
    "T2": "T",
    "Statement": "Biological species evolve.",
    "M proportion correct": 0.98,
    "M response time (ms)": 2809
  },
  {
    "Category": "Evolution",
    "Concept": "Adaptation",
    "T1": "F",
    "T2": "F",
    "Statement": "Inanimate objects evolve.",
    "M proportion correct": 0.84,
    "M response time (ms)": 3527
  },
  {
    "Category": "Evolution",
    "Concept": "Adaptation",
    "T1": "T",
    "T2": "F",
    "Statement": "Individual organisms evolve.",
    "M proportion correct": 0.44,
    "M response time (ms)": 4031
  },
  {
    "Category": "Evolution",
    "Concept": "Adaptation",
    "T1": "F",
    "T2": "T",
    "Statement": "Computer viruses evolve.",
    "M proportion correct": 0.48,
    "M response time (ms)": 3250
  },
  {
    "Category": "Fractions",
    "Concept": "Addition",
    "T1": "T",
    "T2": "T",
    "Statement": "1/3 plus 1/3 is 2/3.",
    "M proportion correct": 0.94,
    "M response time (ms)": 3222
  },
  {
    "Category": "Fractions",
    "Concept": "Addition",
    "T1": "F",
    "T2": "F",
    "Statement": "1/3 plus 1/3 is 1/3.",
    "M proportion correct": 0.97,
    "M response time (ms)": 3285
  },
  {
    "Category": "Fractions",
    "Concept": "Addition",
    "T1": "T",
    "T2": "F",
    "Statement": "2/3 plus 2/4 is 2/7.",
    "M proportion correct": 0.91,
    "M response time (ms)": 5263
  },
  {
    "Category": "Fractions",
    "Concept": "Addition",
    "T1": "F",
    "T2": "T",
    "Statement": "1/10 plus 1/10 is 1/5.",
    "M proportion correct": 0.86,
    "M response time (ms)": 4883
  },
  {
    "Category": "Fractions",
    "Concept": "Division",
    "T1": "T",
    "T2": "T",
    "Statement": "4 divided by 2/1 is 2.",
    "M proportion correct": 0.98,
    "M response time (ms)": 2659
  },
  {
    "Category": "Fractions",
    "Concept": "Division",
    "T1": "F",
    "T2": "F",
    "Statement": "8 divided by 2/1 is 3.",
    "M proportion correct": 0.99,
    "M response time (ms)": 2967
  },
  {
    "Category": "Fractions",
    "Concept": "Division",
    "T1": "T",
    "T2": "F",
    "Statement": "8 divided by 1/2 is 4.",
    "M proportion correct": 0.71,
    "M response time (ms)": 4583
  },
  {
    "Category": "Fractions",
    "Concept": "Division",
    "T1": "F",
    "T2": "T",
    "Statement": "4 divided by 1/2 is 8.",
    "M proportion correct": 0.75,
    "M response time (ms)": 4207
  },
  {
    "Category": "Fractions",
    "Concept": "Conversion",
    "T1": "T",
    "T2": "T",
    "Statement": "All decimals are expressible as fractions.",
    "M proportion correct": 0.71,
    "M response time (ms)": 4233
  },
  {
    "Category": "Fractions",
    "Concept": "Conversion",
    "T1": "F",
    "T2": "F",
    "Statement": "All fractions are expressible as integers.",
    "M proportion correct": 0.71,
    "M response time (ms)": 4481
  },
  {
    "Category": "Fractions",
    "Concept": "Conversion",
    "T1": "T",
    "T2": "F",
    "Statement": "All fractions are expressible as decimals.",
    "M proportion correct": 0.1,
    "M response time (ms)": 5375
  },
  {
    "Category": "Fractions",
    "Concept": "Conversion",
    "T1": "F",
    "T2": "T",
    "Statement": "All integers are expressible as fractions.",
    "M proportion correct": 0.79,
    "M response time (ms)": 4506
  },
  {
    "Category": "Fractions",
    "Concept": "Ordering",
    "T1": "T",
    "T2": "T",
    "Statement": "12/13 is greater than 1/13.",
    "M proportion correct": 0.97,
    "M response time (ms)": 2808
  },
  {
    "Category": "Fractions",
    "Concept": "Ordering",
    "T1": "F",
    "T2": "F",
    "Statement": "1/17 is greater than 16/17.",
    "M proportion correct": 0.96,
    "M response time (ms)": 4177
  },
  {
    "Category": "Fractions",
    "Concept": "Ordering",
    "T1": "T",
    "T2": "F",
    "Statement": "1/17 is greater than 1/9.",
    "M proportion correct": 0.94,
    "M response time (ms)": 5472
  },
  {
    "Category": "Fractions",
    "Concept": "Ordering",
    "T1": "F",
    "T2": "T",
    "Statement": "1/13 is greater than 1/30.",
    "M proportion correct": 0.94,
    "M response time (ms)": 3921
  },
  {
    "Category": "Fractions",
    "Concept": "Infinite density",
    "T1": "T",
    "T2": "T",
    "Statement": "There are numbers between 1 and 3.",
    "M proportion correct": 0.96,
    "M response time (ms)": 3679
  },
  {
    "Category": "Fractions",
    "Concept": "Infinite density",
    "T1": "F",
    "T2": "F",
    "Statement": "There are numbers between 10 and 10.",
    "M proportion correct": 0.75,
    "M response time (ms)": 3893
  },
  {
    "Category": "Fractions",
    "Concept": "Infinite density",
    "T1": "T",
    "T2": "F",
    "Statement": "There are numbers between 1/10 and 10/100.",
    "M proportion correct": 0.52,
    "M response time (ms)": 3685
  },
  {
    "Category": "Fractions",
    "Concept": "Infinite density",
    "T1": "F",
    "T2": "T",
    "Statement": "There are numbers between 1/3 and 1/2.",
    "M proportion correct": 0.93,
    "M response time (ms)": 3731
  },
  {
    "Category": "Genetics",
    "Concept": "Heritability",
    "T1": "T",
    "T2": "T",
    "Statement": "Hair color is heritable.",
    "M proportion correct": 0.98,
    "M response time (ms)": 2760
  },
  {
    "Category": "Genetics",
    "Concept": "Heritability",
    "T1": "F",
    "T2": "F",
    "Statement": "Pierced ears are heritable.",
    "M proportion correct": 0.98,
    "M response time (ms)": 2774
  },
  {
    "Category": "Genetics",
    "Concept": "Heritability",
    "T1": "T",
    "T2": "F",
    "Statement": "Immunity to chickenpox is heritable.",
    "M proportion correct": 0.68,
    "M response time (ms)": 3904
  },
  {
    "Category": "Genetics",
    "Concept": "Heritability",
    "T1": "F",
    "T2": "T",
    "Statement": "Intelligence is heritable.",
    "M proportion correct": 0.54,
    "M response time (ms)": 3287
  },
  {
    "Category": "Genetics",
    "Concept": "Chromosome",
    "T1": "T",
    "T2": "T",
    "Statement": "Humans have more chromosomes than mosquitoes.",
    "M proportion correct": 0.74,
    "M response time (ms)": 3988
  },
  {
    "Category": "Genetics",
    "Concept": "Chromosome",
    "T1": "F",
    "T2": "F",
    "Statement": "Men have more chromosomes than women.",
    "M proportion correct": 0.85,
    "M response time (ms)": 3751
  },
  {
    "Category": "Genetics",
    "Concept": "Chromosome",
    "T1": "T",
    "T2": "F",
    "Statement": "Adults have more chromosomes than children.",
    "M proportion correct": 0.91,
    "M response time (ms)": 3539
  },
  {
    "Category": "Genetics",
    "Concept": "Chromosome",
    "T1": "F",
    "T2": "T",
    "Statement": "Skin cells have more chromosomes than egg cells.",
    "M proportion correct": 0.47,
    "M response time (ms)": 5255
  },
  {
    "Category": "Genetics",
    "Concept": "Dominance",
    "T1": "T",
    "T2": "T",
    "Statement": "Two brown-eyed parents could have a brown-eyed baby.",
    "M proportion correct": 0.97,
    "M response time (ms)": 3369
  },
  {
    "Category": "Genetics",
    "Concept": "Dominance",
    "T1": "F",
    "T2": "F",
    "Statement": "Two blue-eyed parents could have a brown-eyed baby.",
    "M proportion correct": 0.9,
    "M response time (ms)": 4730
  },
  {
    "Category": "Genetics",
    "Concept": "Dominance",
    "T1": "T",
    "T2": "F",
    "Statement": "A blue-eyed baby must have two blue-eyed parents.",
    "M proportion correct": 0.85,
    "M response time (ms)": 3632
  },
  {
    "Category": "Genetics",
    "Concept": "Dominance",
    "T1": "F",
    "T2": "T",
    "Statement": "Two brown-eyed parents could have a blue-eyed baby.",
    "M proportion correct": 0.23,
    "M response time (ms)": 3294
  },
  {
    "Category": "Genetics",
    "Concept": "Expression",
    "T1": "T",
    "T2": "T",
    "Statement": "Genes that code for eye color can be found in the eye.",
    "M proportion correct": 0.85,
    "M response time (ms)": 4996
  },
  {
    "Category": "Genetics",
    "Concept": "Expression",
    "T1": "F",
    "T2": "F",
    "Statement": "Genes that code for eye color can be found in hair.",
    "M proportion correct": 0.48,
    "M response time (ms)": 3875
  },
  {
    "Category": "Genetics",
    "Concept": "Expression",
    "T1": "T",
    "T2": "F",
    "Statement": "Genes that code for eye color can be found in tears.",
    "M proportion correct": 0.71,
    "M response time (ms)": 4114
  },
  {
    "Category": "Genetics",
    "Concept": "Expression",
    "T1": "F",
    "T2": "T",
    "Statement": "Genes that code for eye color can be found in the liver.",
    "M proportion correct": 0.38,
    "M response time (ms)": 4626
  },
  {
    "Category": "Genetics",
    "Concept": "Mutation",
    "T1": "T",
    "T2": "T",
    "Statement": "Radiation can change one's genome.",
    "M proportion correct": 0.79,
    "M response time (ms)": 3365
  },
  {
    "Category": "Genetics",
    "Concept": "Mutation",
    "T1": "F",
    "T2": "F",
    "Statement": "Sunscreen can change one's genome.",
    "M proportion correct": 0.93,
    "M response time (ms)": 3213
  },
  {
    "Category": "Genetics",
    "Concept": "Mutation",
    "T1": "T",
    "T2": "F",
    "Statement": "Exercise can change one's genome.",
    "M proportion correct": 0.81,
    "M response time (ms)": 3553
  },
  {
    "Category": "Genetics",
    "Concept": "Mutation",
    "T1": "F",
    "T2": "T",
    "Statement": "Viruses can change one's genome.",
    "M proportion correct": 0.53,
    "M response time (ms)": 3602
  },
  {
    "Category": "Germs",
    "Concept": "Contagion",
    "T1": "T",
    "T2": "T",
    "Statement": "Being sneezed on can make a person sick.",
    "M proportion correct": 0.95,
    "M response time (ms)": 2870
  },
  {
    "Category": "Germs",
    "Concept": "Contagion",
    "T1": "F",
    "T2": "F",
    "Statement": "Being happy can make a person sick.",
    "M proportion correct": 0.89,
    "M response time (ms)": 3447
  },
  {
    "Category": "Germs",
    "Concept": "Contagion",
    "T1": "T",
    "T2": "F",
    "Statement": "Being cold can make a person sick.",
    "M proportion correct": 0.36,
    "M response time (ms)": 3468
  },
  {
    "Category": "Germs",
    "Concept": "Contagion",
    "T1": "F",
    "T2": "T",
    "Statement": "Being depressed can make a person sick.",
    "M proportion correct": 0.91,
    "M response time (ms)": 2840
  },
  {
    "Category": "Germs",
    "Concept": "Contamination",
    "T1": "T",
    "T2": "T",
    "Statement": "Rotting meat contains germs.",
    "M proportion correct": 0.96,
    "M response time (ms)": 2638
  },
  {
    "Category": "Germs",
    "Concept": "Contamination",
    "T1": "F",
    "T2": "F",
    "Statement": "Sunshine contains germs.",
    "M proportion correct": 0.89,
    "M response time (ms)": 2990
  },
  {
    "Category": "Germs",
    "Concept": "Contamination",
    "T1": "T",
    "T2": "F",
    "Statement": "Urine contains germs.",
    "M proportion correct": 0.27,
    "M response time (ms)": 3780
  },
  {
    "Category": "Germs",
    "Concept": "Contamination",
    "T1": "F",
    "T2": "T",
    "Statement": "Dish sponges contain germs.",
    "M proportion correct": 0.96,
    "M response time (ms)": 2919
  },
  {
    "Category": "Germs",
    "Concept": "Infection",
    "T1": "T",
    "T2": "T",
    "Statement": "Germs enter the body through cuts.",
    "M proportion correct": 0.97,
    "M response time (ms)": 2553
  },
  {
    "Category": "Germs",
    "Concept": "Infection",
    "T1": "F",
    "T2": "F",
    "Statement": "Germs enter the body through the hair.",
    "M proportion correct": 0.77,
    "M response time (ms)": 3535
  },
  {
    "Category": "Germs",
    "Concept": "Infection",
    "T1": "T",
    "T2": "F",
    "Statement": "Germs enter the body through the skin.",
    "M proportion correct": 0.57,
    "M response time (ms)": 4172
  },
  {
    "Category": "Germs",
    "Concept": "Infection",
    "T1": "F",
    "T2": "T",
    "Statement": "Germs enter the body through the eyes.",
    "M proportion correct": 0.83,
    "M response time (ms)": 2797
  },
  {
    "Category": "Germs",
    "Concept": "Sterilization",
    "T1": "T",
    "T2": "T",
    "Statement": "Alcohol kills germs.",
    "M proportion correct": 0.91,
    "M response time (ms)": 2377
  },
  {
    "Category": "Germs",
    "Concept": "Sterilization",
    "T1": "F",
    "T2": "F",
    "Statement": "Kindness kills germs.",
    "M proportion correct": 0.95,
    "M response time (ms)": 2662
  },
  {
    "Category": "Germs",
    "Concept": "Sterilization",
    "T1": "T",
    "T2": "F",
    "Statement": "Water kills germs.",
    "M proportion correct": 0.83,
    "M response time (ms)": 3058
  },
  {
    "Category": "Germs",
    "Concept": "Sterilization",
    "T1": "F",
    "T2": "T",
    "Statement": "Heat kills germs.",
    "M proportion correct": 0.89,
    "M response time (ms)": 2647
  },
  {
    "Category": "Germs",
    "Concept": "Microbe",
    "T1": "T",
    "T2": "T",
    "Statement": "Germs have a shape.",
    "M proportion correct": 0.83,
    "M response time (ms)": 2652
  },
  {
    "Category": "Germs",
    "Concept": "Microbe",
    "T1": "F",
    "T2": "F",
    "Statement": "Germs have feelings.",
    "M proportion correct": 0.97,
    "M response time (ms)": 2053
  },
  {
    "Category": "Germs",
    "Concept": "Microbe",
    "T1": "T",
    "T2": "F",
    "Statement": "Germs have an odor.",
    "M proportion correct": 0.65,
    "M response time (ms)": 2996
  },
  {
    "Category": "Germs",
    "Concept": "Microbe",
    "T1": "F",
    "T2": "T",
    "Statement": "Germs have DNA.",
    "M proportion correct": 0.71,
    "M response time (ms)": 2639
  },
  {
    "Category": "Matter",
    "Concept": "Mass",
    "T1": "T",
    "T2": "T",
    "Statement": "Rocks are composed of matter.",
    "M proportion correct": 0.97,
    "M response time (ms)": 2521
  },
  {
    "Category": "Matter",
    "Concept": "Mass",
    "T1": "F",
    "T2": "F",
    "Statement": "Numbers are composed of matter.",
    "M proportion correct": 0.9,
    "M response time (ms)": 3386
  },
  {
    "Category": "Matter",
    "Concept": "Mass",
    "T1": "T",
    "T2": "F",
    "Statement": "Fire is composed of matter.",
    "M proportion correct": 0.29,
    "M response time (ms)": 3860
  },
  {
    "Category": "Matter",
    "Concept": "Mass",
    "T1": "F",
    "T2": "T",
    "Statement": "Air is composed of matter.",
    "M proportion correct": 0.87,
    "M response time (ms)": 2696
  },
  {
    "Category": "Matter",
    "Concept": "Weight",
    "T1": "T",
    "T2": "T",
    "Statement": "A liter of water weighs more than a liter of air.",
    "M proportion correct": 0.62,
    "M response time (ms)": 4771
  },
  {
    "Category": "Matter",
    "Concept": "Weight",
    "T1": "F",
    "T2": "F",
    "Statement": "A pound of steel weighs more than a ton of steel.",
    "M proportion correct": 0.94,
    "M response time (ms)": 4833
  },
  {
    "Category": "Matter",
    "Concept": "Weight",
    "T1": "T",
    "T2": "F",
    "Statement": "A pound of steel weighs more than a pound of feathers.",
    "M proportion correct": 0.66,
    "M response time (ms)": 3806
  },
  {
    "Category": "Matter",
    "Concept": "Weight",
    "T1": "F",
    "T2": "T",
    "Statement": "A liter of water weighs more than a liter of ice.",
    "M proportion correct": 0.29,
    "M response time (ms)": 6172
  },
  {
    "Category": "Matter",
    "Concept": "Density",
    "T1": "T",
    "T2": "T",
    "Statement": "Steel is denser than foam.",
    "M proportion correct": 0.95,
    "M response time (ms)": 3425
  },
  {
    "Category": "Matter",
    "Concept": "Density",
    "T1": "F",
    "T2": "F",
    "Statement": "Foam is denser than brick.",
    "M proportion correct": 0.94,
    "M response time (ms)": 3598
  },
  {
    "Category": "Matter",
    "Concept": "Density",
    "T1": "T",
    "T2": "F",
    "Statement": "Ice is denser than water.",
    "M proportion correct": 0.37,
    "M response time (ms)": 4163
  },
  {
    "Category": "Matter",
    "Concept": "Density",
    "T1": "F",
    "T2": "T",
    "Statement": "A cold penny is denser than a hot penny.",
    "M proportion correct": 0.5,
    "M response time (ms)": 4982
  },
  {
    "Category": "Matter",
    "Concept": "Divisibility",
    "T1": "T",
    "T2": "T",
    "Statement": "A log can be cut in half.",
    "M proportion correct": 0.99,
    "M response time (ms)": 2563
  },
  {
    "Category": "Matter",
    "Concept": "Divisibility",
    "T1": "F",
    "T2": "F",
    "Statement": "An idea can be cut in half.",
    "M proportion correct": 0.85,
    "M response time (ms)": 2956
  },
  {
    "Category": "Matter",
    "Concept": "Divisibility",
    "T1": "T",
    "T2": "F",
    "Statement": "A shadow can be cut in half.",
    "M proportion correct": 0.87,
    "M response time (ms)": 2955
  },
  {
    "Category": "Matter",
    "Concept": "Divisibility",
    "T1": "F",
    "T2": "T",
    "Statement": "A grain of sand can be cut in half.",
    "M proportion correct": 0.91,
    "M response time (ms)": 3299
  },
  {
    "Category": "Matter",
    "Concept": "Atom",
    "T1": "T",
    "T2": "T",
    "Statement": "Atoms are the constituents of matter.",
    "M proportion correct": 0.99,
    "M response time (ms)": 2138
  },
  {
    "Category": "Matter",
    "Concept": "Atom",
    "T1": "F",
    "T2": "F",
    "Statement": "Atoms are visible to the naked eye.",
    "M proportion correct": 0.91,
    "M response time (ms)": 2978
  },
  {
    "Category": "Matter",
    "Concept": "Atom",
    "T1": "T",
    "T2": "F",
    "Statement": "Atoms are weightless.",
    "M proportion correct": 0.89,
    "M response time (ms)": 2950
  },
  {
    "Category": "Matter",
    "Concept": "Atom",
    "T1": "F",
    "T2": "T",
    "Statement": "Atoms are mostly empty space.",
    "M proportion correct": 0.55,
    "M response time (ms)": 3537
  },
  {
    "Category": "Mechanics",
    "Concept": "Force",
    "T1": "T",
    "T2": "T",
    "Statement": "Forces can be strong or weak.",
    "M proportion correct": 0.97,
    "M response time (ms)": 2629
  },
  {
    "Category": "Mechanics",
    "Concept": "Force",
    "T1": "F",
    "T2": "F",
    "Statement": "Forces can be male or female.",
    "M proportion correct": 0.85,
    "M response time (ms)": 2720
  },
  {
    "Category": "Mechanics",
    "Concept": "Force",
    "T1": "T",
    "T2": "F",
    "Statement": "Forces can be straight or curved.",
    "M proportion correct": 0.36,
    "M response time (ms)": 4360
  },
  {
    "Category": "Mechanics",
    "Concept": "Force",
    "T1": "F",
    "T2": "T",
    "Statement": "Forces can be balanced or unbalanced.",
    "M proportion correct": 0.82,
    "M response time (ms)": 3214
  },
  {
    "Category": "Mechanics",
    "Concept": "Velocity",
    "T1": "T",
    "T2": "T",
    "Statement": "A moving bullet loses speed.",
    "M proportion correct": 0.86,
    "M response time (ms)": 3677
  },
  {
    "Category": "Mechanics",
    "Concept": "Velocity",
    "T1": "F",
    "T2": "F",
    "Statement": "A moving bullet loses weight.",
    "M proportion correct": 0.83,
    "M response time (ms)": 3976
  },
  {
    "Category": "Mechanics",
    "Concept": "Velocity",
    "T1": "T",
    "T2": "F",
    "Statement": "A moving bullet loses force.",
    "M proportion correct": 0.19,
    "M response time (ms)": 4878
  },
  {
    "Category": "Mechanics",
    "Concept": "Velocity",
    "T1": "F",
    "T2": "T",
    "Statement": "A moving bullet loses height.",
    "M proportion correct": 0.7,
    "M response time (ms)": 4133
  },
  {
    "Category": "Mechanics",
    "Concept": "Acceleration",
    "T1": "T",
    "T2": "T",
    "Statement": "Constant acceleration requires constant force.",
    "M proportion correct": 0.81,
    "M response time (ms)": 4375
  },
  {
    "Category": "Mechanics",
    "Concept": "Acceleration",
    "T1": "F",
    "T2": "F",
    "Statement": "Constant speed requires constant acceleration.",
    "M proportion correct": 0.61,
    "M response time (ms)": 5516
  },
  {
    "Category": "Mechanics",
    "Concept": "Acceleration",
    "T1": "T",
    "T2": "F",
    "Statement": "Constant motion requires constant force.",
    "M proportion correct": 0.28,
    "M response time (ms)": 4845
  },
  {
    "Category": "Mechanics",
    "Concept": "Acceleration",
    "T1": "F",
    "T2": "T",
    "Statement": "Constant force can yield constant rest.",
    "M proportion correct": 0.45,
    "M response time (ms)": 5630
  },
  {
    "Category": "Mechanics",
    "Concept": "Momentum",
    "T1": "T",
    "T2": "T",
    "Statement": "Momentum can be transferred from one object to another.",
    "M proportion correct": 0.87,
    "M response time (ms)": 3610
  },
  {
    "Category": "Mechanics",
    "Concept": "Momentum",
    "T1": "F",
    "T2": "F",
    "Statement": "Color can be transferred from one object to another.",
    "M proportion correct": 0.66,
    "M response time (ms)": 3963
  },
  {
    "Category": "Mechanics",
    "Concept": "Momentum",
    "T1": "T",
    "T2": "F",
    "Statement": "Forces can be transferred from one object to another.",
    "M proportion correct": 0.16,
    "M response time (ms)": 4968
  },
  {
    "Category": "Mechanics",
    "Concept": "Momentum",
    "T1": "F",
    "T2": "T",
    "Statement": "Energy can be transferred from one object to another.",
    "M proportion correct": 0.95,
    "M response time (ms)": 3096
  },
  {
    "Category": "Mechanics",
    "Concept": "Gravity",
    "T1": "T",
    "T2": "T",
    "Statement": "Anvils fall through air faster than feathers.",
    "M proportion correct": 0.69,
    "M response time (ms)": 4463
  },
  {
    "Category": "Mechanics",
    "Concept": "Gravity",
    "T1": "F",
    "T2": "F",
    "Statement": "Bright objects fall through air faster than dark objects.",
    "M proportion correct": 0.97,
    "M response time (ms)": 4329
  },
  {
    "Category": "Mechanics",
    "Concept": "Gravity",
    "T1": "T",
    "T2": "F",
    "Statement": "Heavy balls fall through air faster than light balls.",
    "M proportion correct": 0.4,
    "M response time (ms)": 4544
  },
  {
    "Category": "Mechanics",
    "Concept": "Gravity",
    "T1": "F",
    "T2": "T",
    "Statement": "Pointy objects fall through air faster than flat objects.",
    "M proportion correct": 0.68,
    "M response time (ms)": 4955
  },
  {
    "Category": "Physiology",
    "Concept": "Life",
    "T1": "T",
    "T2": "T",
    "Statement": "Fish are alive.",
    "M proportion correct": 0.97,
    "M response time (ms)": 1618
  },
  {
    "Category": "Physiology",
    "Concept": "Life",
    "T1": "F",
    "T2": "F",
    "Statement": "Rocks are alive.",
    "M proportion correct": 0.92,
    "M response time (ms)": 2144
  },
  {
    "Category": "Physiology",
    "Concept": "Life",
    "T1": "T",
    "T2": "F",
    "Statement": "The sun is alive.",
    "M proportion correct": 0.93,
    "M response time (ms)": 2716
  },
  {
    "Category": "Physiology",
    "Concept": "Life",
    "T1": "F",
    "T2": "T",
    "Statement": "Coral is alive.",
    "M proportion correct": 0.68,
    "M response time (ms)": 2026
  },
  {
    "Category": "Physiology",
    "Concept": "Death",
    "T1": "T",
    "T2": "T",
    "Statement": "Turtles can die.",
    "M proportion correct": 0.97,
    "M response time (ms)": 1827
  },
  {
    "Category": "Physiology",
    "Concept": "Death",
    "T1": "F",
    "T2": "F",
    "Statement": "Screwdrivers can die.",
    "M proportion correct": 0.99,
    "M response time (ms)": 2405
  },
  {
    "Category": "Physiology",
    "Concept": "Death",
    "T1": "T",
    "T2": "F",
    "Statement": "Clouds can die.",
    "M proportion correct": 0.93,
    "M response time (ms)": 2585
  },
  {
    "Category": "Physiology",
    "Concept": "Death",
    "T1": "F",
    "T2": "T",
    "Statement": "Mushrooms can die.",
    "M proportion correct": 0.93,
    "M response time (ms)": 2454
  },
  {
    "Category": "Physiology",
    "Concept": "Reproduction",
    "T1": "T",
    "T2": "T",
    "Statement": "Tigers can reproduce.",
    "M proportion correct": 0.99,
    "M response time (ms)": 2761
  },
  {
    "Category": "Physiology",
    "Concept": "Reproduction",
    "T1": "F",
    "T2": "F",
    "Statement": "Chairs can reproduce.",
    "M proportion correct": 0.96,
    "M response time (ms)": 3218
  },
  {
    "Category": "Physiology",
    "Concept": "Reproduction",
    "T1": "T",
    "T2": "F",
    "Statement": "Caterpillars can reproduce.",
    "M proportion correct": 0.61,
    "M response time (ms)": 4615
  },
  {
    "Category": "Physiology",
    "Concept": "Reproduction",
    "T1": "F",
    "T2": "T",
    "Statement": "Ferns can reproduce.",
    "M proportion correct": 0.72,
    "M response time (ms)": 3666
  },
  {
    "Category": "Physiology",
    "Concept": "Metabolism",
    "T1": "T",
    "T2": "T",
    "Statement": "People turn food into energy.",
    "M proportion correct": 0.96,
    "M response time (ms)": 2620
  },
  {
    "Category": "Physiology",
    "Concept": "Metabolism",
    "T1": "F",
    "T2": "F",
    "Statement": "Rocks turn food into energy.",
    "M proportion correct": 0.93,
    "M response time (ms)": 3832
  },
  {
    "Category": "Physiology",
    "Concept": "Metabolism",
    "T1": "T",
    "T2": "F",
    "Statement": "Plants turn food into energy.",
    "M proportion correct": 0.75,
    "M response time (ms)": 5194
  },
  {
    "Category": "Physiology",
    "Concept": "Metabolism",
    "T1": "F",
    "T2": "T",
    "Statement": "Bacteria turn food into energy.",
    "M proportion correct": 0.75,
    "M response time (ms)": 3758
  },
  {
    "Category": "Physiology",
    "Concept": "Kinship",
    "T1": "T",
    "T2": "T",
    "Statement": "A baby can be a niece or a nephew.",
    "M proportion correct": 0.93,
    "M response time (ms)": 3395
  },
  {
    "Category": "Physiology",
    "Concept": "Kinship",
    "T1": "F",
    "T2": "F",
    "Statement": "A baby can be a mother or a father.",
    "M proportion correct": 0.8,
    "M response time (ms)": 4250
  },
  {
    "Category": "Physiology",
    "Concept": "Kinship",
    "T1": "T",
    "T2": "F",
    "Statement": "A baby can be identical to an older sibling.",
    "M proportion correct": 0.81,
    "M response time (ms)": 4489
  },
  {
    "Category": "Physiology",
    "Concept": "Kinship",
    "T1": "F",
    "T2": "T",
    "Statement": "A baby can be an uncle or an aunt.",
    "M proportion correct": 0.81,
    "M response time (ms)": 4024
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Heat",
    "T1": "T",
    "T2": "T",
    "Statement": "The sun has heat.",
    "M proportion correct": 0.99,
    "M response time (ms)": 2223
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Heat",
    "T1": "F",
    "T2": "F",
    "Statement": "Gravity has heat.",
    "M proportion correct": 0.85,
    "M response time (ms)": 2595
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Heat",
    "T1": "T",
    "T2": "F",
    "Statement": "Atoms have heat.",
    "M proportion correct": 0.21,
    "M response time (ms)": 3451
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Heat",
    "T1": "F",
    "T2": "T",
    "Statement": "Ice has heat.",
    "M proportion correct": 0.66,
    "M response time (ms)": 2989
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Heat source",
    "T1": "T",
    "T2": "T",
    "Statement": "Ovens produce heat.",
    "M proportion correct": 0.97,
    "M response time (ms)": 2819
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Heat source",
    "T1": "F",
    "T2": "F",
    "Statement": "Rain produces heat.",
    "M proportion correct": 0.7,
    "M response time (ms)": 3109
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Heat source",
    "T1": "T",
    "T2": "F",
    "Statement": "Coats produce heat.",
    "M proportion correct": 0.71,
    "M response time (ms)": 3863
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Heat source",
    "T1": "F",
    "T2": "T",
    "Statement": "Pressure produces heat.",
    "M proportion correct": 0.73,
    "M response time (ms)": 3470
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Heat transfer",
    "T1": "T",
    "T2": "T",
    "Statement": "Water transfers heat to ice.",
    "M proportion correct": 0.6,
    "M response time (ms)": 4710
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Heat transfer",
    "T1": "F",
    "T2": "F",
    "Statement": "Cold objects transfer heat to warm objects.",
    "M proportion correct": 0.85,
    "M response time (ms)": 4877
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Heat transfer",
    "T1": "T",
    "T2": "F",
    "Statement": "Water transfers heat to steam.",
    "M proportion correct": 0.53,
    "M response time (ms)": 5870
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Heat transfer",
    "T1": "F",
    "T2": "T",
    "Statement": "Cold objects transfer heat to even colder objects.",
    "M proportion correct": 0.72,
    "M response time (ms)": 5279
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Temperature",
    "T1": "T",
    "T2": "T",
    "Statement": "Ice has a lower temperature than water.",
    "M proportion correct": 0.92,
    "M response time (ms)": 3744
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Temperature",
    "T1": "F",
    "T2": "F",
    "Statement": "Steam has a lower temperature than ice.",
    "M proportion correct": 0.92,
    "M response time (ms)": 4382
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Temperature",
    "T1": "T",
    "T2": "F",
    "Statement": "Two cups of ice have a lower temperature than one.",
    "M proportion correct": 0.77,
    "M response time (ms)": 5282
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Temperature",
    "T1": "F",
    "T2": "T",
    "Statement": "Boiling water has a lower temperature than steam.",
    "M proportion correct": 0.56,
    "M response time (ms)": 5067
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Thermal expansion",
    "T1": "T",
    "T2": "T",
    "Statement": "Heat increases an object's temperature.",
    "M proportion correct": 0.95,
    "M response time (ms)": 3919
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Thermal expansion",
    "T1": "F",
    "T2": "F",
    "Statement": "Heat increases an object's color.",
    "M proportion correct": 0.73,
    "M response time (ms)": 3960
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Thermal expansion",
    "T1": "T",
    "T2": "F",
    "Statement": "Heat increases an object's weight.",
    "M proportion correct": 0.89,
    "M response time (ms)": 4111
  },
  {
    "Category": "Thermodynamics",
    "Concept": "Thermal expansion",
    "T1": "F",
    "T2": "T",
    "Statement": "Heat increases an object's size.",
    "M proportion correct": 0.58,
    "M response time (ms)": 3965
  },
  {
    "Category": "Waves",
    "Concept": "Light",
    "T1": "T",
    "T2": "T",
    "Statement": "Rainbows contain all colors of light.",
    "M proportion correct": 0.75,
    "M response time (ms)": 4145
  },
  {
    "Category": "Waves",
    "Concept": "Light",
    "T1": "F",
    "T2": "F",
    "Statement": "Shadows contain all colors of light.",
    "M proportion correct": 0.95,
    "M response time (ms)": 2973
  },
  {
    "Category": "Waves",
    "Concept": "Light",
    "T1": "T",
    "T2": "F",
    "Statement": "Black objects reflect all colors of light.",
    "M proportion correct": 0.67,
    "M response time (ms)": 4599
  },
  {
    "Category": "Waves",
    "Concept": "Light",
    "T1": "F",
    "T2": "T",
    "Statement": "White light contains all colors of light.",
    "M proportion correct": 0.75,
    "M response time (ms)": 3653
  },
  {
    "Category": "Waves",
    "Concept": "Color",
    "T1": "T",
    "T2": "T",
    "Statement": "Red objects reflect red light.",
    "M proportion correct": 0.69,
    "M response time (ms)": 3531
  },
  {
    "Category": "Waves",
    "Concept": "Color",
    "T1": "F",
    "T2": "F",
    "Statement": "Red objects reflect blue light.",
    "M proportion correct": 0.7,
    "M response time (ms)": 4008
  },
  {
    "Category": "Waves",
    "Concept": "Color",
    "T1": "T",
    "T2": "F",
    "Statement": "Red objects absorb red light.",
    "M proportion correct": 0.64,
    "M response time (ms)": 3695
  },
  {
    "Category": "Waves",
    "Concept": "Color",
    "T1": "F",
    "T2": "T",
    "Statement": "Red objects absorb blue light.",
    "M proportion correct": 0.61,
    "M response time (ms)": 3711
  },
  {
    "Category": "Waves",
    "Concept": "Sound",
    "T1": "T",
    "T2": "T",
    "Statement": "Sounds can be loud or quiet.",
    "M proportion correct": 0.94,
    "M response time (ms)": 2894
  },
  {
    "Category": "Waves",
    "Concept": "Sound",
    "T1": "F",
    "T2": "F",
    "Statement": "Sounds can be dead or alive.",
    "M proportion correct": 0.88,
    "M response time (ms)": 3066
  },
  {
    "Category": "Waves",
    "Concept": "Sound",
    "T1": "T",
    "T2": "F",
    "Statement": "Sounds can be near or far.",
    "M proportion correct": 0.12,
    "M response time (ms)": 3467
  },
  {
    "Category": "Waves",
    "Concept": "Sound",
    "T1": "F",
    "T2": "T",
    "Statement": "Sounds can be direct or reflected.",
    "M proportion correct": 0.97,
    "M response time (ms)": 3827
  },
  {
    "Category": "Waves",
    "Concept": "Propagation",
    "T1": "T",
    "T2": "T",
    "Statement": "Sound travels through air.",
    "M proportion correct": 0.98,
    "M response time (ms)": 2516
  },
  {
    "Category": "Waves",
    "Concept": "Propagation",
    "T1": "F",
    "T2": "F",
    "Statement": "Sound travels through foam.",
    "M proportion correct": 0.57,
    "M response time (ms)": 3763
  },
  {
    "Category": "Waves",
    "Concept": "Propagation",
    "T1": "T",
    "T2": "F",
    "Statement": "Sound travels through a vacuum.",
    "M proportion correct": 0.27,
    "M response time (ms)": 3377
  },
  {
    "Category": "Waves",
    "Concept": "Propagation",
    "T1": "F",
    "T2": "T",
    "Statement": "Sound travels through metal.",
    "M proportion correct": 0.78,
    "M response time (ms)": 3418
  },
  {
    "Category": "Waves",
    "Concept": "Reflection",
    "T1": "T",
    "T2": "T",
    "Statement": "Mirrors reflect light.",
    "M proportion correct": 0.82,
    "M response time (ms)": 4787
  },
  {
    "Category": "Waves",
    "Concept": "Reflection",
    "T1": "F",
    "T2": "F",
    "Statement": "Foam reflects sound.",
    "M proportion correct": 0.93,
    "M response time (ms)": 4269
  },
  {
    "Category": "Waves",
    "Concept": "Reflection",
    "T1": "T",
    "T2": "F",
    "Statement": "Prisms reflect light.",
    "M proportion correct": 0.29,
    "M response time (ms)": 5533
  },
  {
    "Category": "Waves",
    "Concept": "Reflection",
    "T1": "F",
    "T2": "T",
    "Statement": "Mirrors reflect sound.",
    "M proportion correct": 0.53,
    "M response time (ms)": 4978
  }
]