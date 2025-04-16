{- |
Module      : HwData.Values
Description : Values of pets, heros, titans
Copyright   : (c) Stratis Christodoulou 2025
Maintainer  : stratis.vip@gmail.com
Stability   : experimental

This module holds all data of the main characters of the game

Original date: 6 Apr 2025
-}
module HwData.Values (allPets, allHeros, allTitans) where

import Typesold (Hero (..), Pet (..), Titan (..))

-- | Data of pets in game
allPets :: [Pet]
allPets =
  [ Pet ("###", "No Pet", 0)
  , Pet ("Alb", "Albus", 1)
  , Pet ("Axe", "Axel", 2)
  , Pet ("Bis", "Biscuit", 3)
  , Pet ("Cai", "Cain", 4)
  , Pet ("Fen", "Fenris", 5)
  , Pet ("Kho", "Khorus", 6)
  , Pet ("Mar", "Mara", 7)
  , Pet ("Mer", "Merlin", 8)
  , Pet ("Oli", "Oliver", 9)
  , Pet ("Vex", "Vex", 10)
  ]

-- | Data of Titans in game
allTitans :: [Titan]
allTitans =
  [ Titan ("Amo", "Amon", 18)
  , Titan ("Ang", "Angus", 9)
  , Titan ("Ara", "Araji", 8)
  , Titan ("Ava", "Avalon", 11)
  , Titan ("Bru", "Brustar", 13)
  , Titan ("Ede", "Eden", 12)
  , Titan ("Hyp", "Hyperion", 4)
  , Titan ("Ign", "Ignis", 7)
  , Titan ("Iya", "Iyari", 19)
  , Titan ("Ker", "Keros", 14)
  , Titan ("Mai", "Mairi", 3)
  , Titan ("Mol", "Moloch", 5)
  , Titan ("Mor", "Mort", 15)
  , Titan ("Nov", "Nova", 2)
  , Titan ("Rig", "Rigel", 17)
  , Titan ("Sig", "Sigurd", 1)
  , Titan ("Sol", "Solaris", 20)
  , Titan ("Syl", "Sylva", 10)
  , Titan ("Ten", "Tenebris", 16)
  , Titan ("Vul", "Vulcan", 6)
  ]

-- | Data of Heros in game
allHeros :: [Hero]
allHeros =
  [ Hero ("Aid", "Aidan", 58)
  , Hero ("Alv", "Alvanor", 53)
  , Hero ("Ami", "Amira", 56)
  , Hero ("And", "Andvari", 47)
  , Hero ("Ara", "Arachne", 12)
  , Hero ("Art", "Artemis", 20)
  , Hero ("Ast", "Astaroth", 4)
  , Hero ("A&L", "Astrid and Lucas", 44)
  , Hero ("Aug", "Augustus", 64)
  , Hero ("Aur", "Aurora", 1)
  , Hero ("Cel", "Celeste", 43)
  , Hero ("Cha", "Chabba", 11)
  , Hero ("Cle", "Cleaver", 24)
  , Hero ("Krn", "Cornelius", 30)
  , Hero ("Krv", "Corvus", 50)
  , Hero ("Dan", "Dante", 16)
  , Hero ("Dd", "Daredevil", 8)
  , Hero ("Drk", "Dark Star", 19)
  , Hero ("Dor", "Dorian", 29)
  , Hero ("Elm", "Elmir", 38)
  , Hero ("Fac", "Faceless", 10)
  , Hero ("Faf", "Fafnir", 57)
  , Hero ("Fox", "Fox", 14)
  , Hero ("Gal", "Galahad", 2)
  , Hero ("Gin", "Ginger", 15)
  , Hero ("Hei", "Heidi", 9)
  , Hero ("Hel", "Helios", 32)
  , Hero ("Iri", "Iris", 55)
  , Hero ("Isa", "Isaac", 52)
  , Hero ("Ish", "Ishmael", 25)
  , Hero ("Jet", "Jet", 31)
  , Hero ("Jhu", "Jhu", 37)
  , Hero ("Jor", "Jorgen", 35)
  , Hero ("Jud", "Judge", 18)
  , Hero ("Jul", "Julius", 61)
  , Hero ("K'A", "K'arkh", 41)
  , Hero ("Kai", "Kai", 5)
  , Hero ("Kay", "Kayla", 59)
  , Hero ("Kei", "Keira", 3)
  , Hero ("Kri", "Krista", 34)
  , Hero ("LaC", "Lara Croft", 63)
  , Hero ("Lrs", "Lars", 33)
  , Hero ("Lia", "Lian", 23)
  , Hero ("Lil", "Lilith", 26)
  , Hero ("Lut", "Luther", 27)
  , Hero ("Mrk", "Markus", 21)
  , Hero ("Mar", "Martha", 46)
  , Hero ("May", "Maya", 36)
  , Hero ("Moj", "Mojo", 17)
  , Hero ("Mor", "Morrigan", 51)
  , Hero ("Mus", "Mushy and Shroom", 60)
  , Hero ("Neb", "Nebula", 40)
  , Hero ("Nin", "Ninja Turtles", 65)
  , Hero ("Fol", "Folio", 66)
  , Hero ("Ori", "Orion", 13)
  , Hero ("Pep", "Peppy", 22)
  , Hero ("Pho", "Phobos", 6)
  , Hero ("Pol", "Polaris", 62)
  , Hero ("Qin", "Qing Mao", 28)
  , Hero ("Ruf", "Rufus", 42)
  , Hero ("Sat", "Satori", 45)
  , Hero ("Seb", "Sebastian", 48)
  , Hero ("The", "Thea", 7)
  , Hero ("Tri", "Tristan", 54)
  , Hero ("Yas", "Yasmine", 49)
  , Hero ("Zir", "Ziri", 39)
  ]
