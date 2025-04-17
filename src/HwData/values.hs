-- |
-- Module      : HwData.Values
-- Description : Values of pets, heros, titans
-- Copyright   : (c) Stratis Christodoulou 2025
-- Maintainer  : stratis.vip@gmail.com
-- Stability   : experimental
--
-- This module holds all data of the main characters of the game
--
-- Original date: 6 Apr 2025
module HwData.Values (allPets, allHeros, allTitans) where

import Types.Assorted (ID (ID))
import Types.Hero (Hero (..))
import Types.Pet (Pet (..))
import Types.Titan (Titan (..))

-- | Data of pets in game
allPets :: [Pet]
allPets =
  [ Pet ("###", "No Pet", ID 0),
    Pet ("Alb", "Albus", ID 1),
    Pet ("Axe", "Axel", ID 2),
    Pet ("Bis", "Biscuit", ID 3),
    Pet ("Cai", "Cain", ID 4),
    Pet ("Fen", "Fenris", ID 5),
    Pet ("Kho", "Khorus", ID 6),
    Pet ("Mar", "Mara", ID 7),
    Pet ("Mer", "Merlin", ID 8),
    Pet ("Oli", "Oliver", ID 9),
    Pet ("Vex", "Vex", ID 10)
  ]

-- | Data of Titans in game
allTitans :: [Titan]
allTitans =
  [ Titan ("Amo", "Amon", ID 18),
    Titan ("Ang", "Angus", ID 9),
    Titan ("Ara", "Araji", ID 8),
    Titan ("Ava", "Avalon", ID 11),
    Titan ("Bru", "Brustar", ID 13),
    Titan ("Ede", "Eden", ID 12),
    Titan ("Hyp", "Hyperion", ID 4),
    Titan ("Ign", "Ignis", ID 7),
    Titan ("Iya", "Iyari", ID 19),
    Titan ("Ker", "Keros", ID 14),
    Titan ("Mai", "Mairi", ID 3),
    Titan ("Mol", "Moloch", ID 5),
    Titan ("Mor", "Mort", ID 15),
    Titan ("Nov", "Nova", ID 2),
    Titan ("Rig", "Rigel", ID 17),
    Titan ("Sig", "Sigurd", ID 1),
    Titan ("Sol", "Solaris", ID 20),
    Titan ("Syl", "Sylva", ID 10),
    Titan ("Ten", "Tenebris", ID 16),
    Titan ("Vul", "Vulcan", ID 6)
  ]

-- | Data of Heros in game
allHeros :: [Hero]
allHeros =
  [ Hero ("Aid", "Aidan", ID 58),
    Hero ("Alv", "Alvanor", ID 53),
    Hero ("Ami", "Amira", ID 56),
    Hero ("And", "Andvari", ID 47),
    Hero ("Ara", "Arachne", ID 12),
    Hero ("Art", "Artemis", ID 20),
    Hero ("Ast", "Astaroth", ID 4),
    Hero ("A&L", "Astrid and Lucas", ID 44),
    Hero ("Aug", "Augustus", ID 64),
    Hero ("Aur", "Aurora", ID 1),
    Hero ("Cel", "Celeste", ID 43),
    Hero ("Cha", "Chabba", ID 11),
    Hero ("Cle", "Cleaver", ID 24),
    Hero ("Krn", "Cornelius", ID 30),
    Hero ("Krv", "Corvus", ID 50),
    Hero ("Dan", "Dante", ID 16),
    Hero ("Dd", "Daredevil", ID 8),
    Hero ("Drk", "Dark Star", ID 19),
    Hero ("Dor", "Dorian", ID 29),
    Hero ("Elm", "Elmir", ID 38),
    Hero ("Fac", "Faceless", ID 10),
    Hero ("Faf", "Fafnir", ID 57),
    Hero ("Fox", "Fox", ID 14),
    Hero ("Gal", "Galahad", ID 2),
    Hero ("Gin", "Ginger", ID 15),
    Hero ("Hei", "Heidi", ID 9),
    Hero ("Hel", "Helios", ID 32),
    Hero ("Iri", "Iris", ID 55),
    Hero ("Isa", "Isaac", ID 52),
    Hero ("Ish", "Ishmael", ID 25),
    Hero ("Jet", "Jet", ID 31),
    Hero ("Jhu", "Jhu", ID 37),
    Hero ("Jor", "Jorgen", ID 35),
    Hero ("Jud", "Judge", ID 18),
    Hero ("Jul", "Julius", ID 61),
    Hero ("K'A", "K'arkh", ID 41),
    Hero ("Kai", "Kai", ID 5),
    Hero ("Kay", "Kayla", ID 59),
    Hero ("Kei", "Keira", ID 3),
    Hero ("Kri", "Krista", ID 34),
    Hero ("LaC", "Lara Croft", ID 63),
    Hero ("Lrs", "Lars", ID 33),
    Hero ("Lia", "Lian", ID 23),
    Hero ("Lil", "Lilith", ID 26),
    Hero ("Lut", "Luther", ID 27),
    Hero ("Mrk", "Markus", ID 21),
    Hero ("Mar", "Martha", ID 46),
    Hero ("May", "Maya", ID 36),
    Hero ("Moj", "Mojo", ID 17),
    Hero ("Mor", "Morrigan", ID 51),
    Hero ("Mus", "Mushy and Shroom", ID 60),
    Hero ("Neb", "Nebula", ID 40),
    Hero ("Nin", "Ninja Turtles", ID 65),
    Hero ("Fol", "Folio", ID 66),
    Hero ("Ori", "Orion", ID 13),
    Hero ("Pep", "Peppy", ID 22),
    Hero ("Pho", "Phobos", ID 6),
    Hero ("Pol", "Polaris", ID 62),
    Hero ("Qin", "Qing Mao", ID 28),
    Hero ("Ruf", "Rufus", ID 42),
    Hero ("Sat", "Satori", ID 45),
    Hero ("Seb", "Sebastian", ID 48),
    Hero ("The", "Thea", ID 7),
    Hero ("Tri", "Tristan", ID 54),
    Hero ("Yas", "Yasmine", ID 49),
    Hero ("Zir", "Ziri", ID 39)
  ]
