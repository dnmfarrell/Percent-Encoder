{-# LANGUAGE BangPatterns #-}
-- | functions for percent encoding/decoding ByteStrings

module Percent.Encoder (encode, decode) where

import Data.Array
import Data.Word8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M

-- | Percent encode a ByteString

encode :: B.ByteString -> B.ByteString
encode = B.concatMap encodeByte

encodeByte :: Word8 -> B.ByteString
encodeByte w = encoderArray ! w

-- | Percent decode a ByteString

decode :: B.ByteString -> B.ByteString
decode bs
  | B.null bs     = B.empty
  | w == _percent = B.cons c $ decode ts
  | otherwise     = B.cons w $ decode t
  where w = B.head bs
        t = B.tail bs
        c = decoderMap M.! (B.take 2 t)
        ts= B.drop 3 bs

el = map C.pack ["%00","%01","%02","%03","%04","%05","%06","%07","%08","%09","%0A","%0B","%0C","%0D","%0E","%0F","%10","%11","%12","%13","%14","%15","%16","%17","%18","%19","%1A","%1B","%1C","%1D","%1E","%1F","%20","%21","%22","%23","%24","%25","%26","%27","%28","%29","%2A","%2B","%2C","-",".","%2F","0","1","2","3","4","5","6","7","8","9","%3A","%3B","%3C","%3D","%3E","%3F","%40","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","%5B","%5C","%5D","%5E","_","%60","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","%7B","%7C","%7D","~","%7F","%80","%81","%82","%83","%84","%85","%86","%87","%88","%89","%8A","%8B","%8C","%8D","%8E","%8F","%90","%91","%92","%93","%94","%95","%96","%97","%98","%99","%9A","%9B","%9C","%9D","%9E","%9F","%A0","%A1","%A2","%A3","%A4","%A5","%A6","%A7","%A8","%A9","%AA","%AB","%AC","%AD","%AE","%AF","%B0","%B1","%B2","%B3","%B4","%B5","%B6","%B7","%B8","%B9","%BA","%BB","%BC","%BD","%BE","%BF","%C0","%C1","%C2","%C3","%C4","%C5","%C6","%C7","%C8","%C9","%CA","%CB","%CC","%CD","%CE","%CF","%D0","%D1","%D2","%D3","%D4","%D5","%D6","%D7","%D8","%D9","%DA","%DB","%DC","%DD","%DE","%DF","%E0","%E1","%E2","%E3","%E4","%E5","%E6","%E7","%E8","%E9","%EA","%EB","%EC","%ED","%EE","%EF","%F0","%F1","%F2","%F3","%F4","%F5","%F6","%F7","%F8","%F9","%FA","%FB","%FC","%FD","%FE","%FF"]

!encoderArray = listArray (0,255) el

tuplePack :: (String, Int) -> (B.ByteString, Word8)
tuplePack tp = (C.pack t1, t2)
  where t1 = fst tp
        t2 = (fromIntegral (snd tp :: Int)) :: Word8

dl = map tuplePack [("00",0),("01",1),("02",2),("03",3),("04",4),("05",5),("06",6),("07",7),("08",8),("09",9),("0A",10),("0B",11),("0C",12),("0D",13),("0E",14),("0F",15),("10",16),("11",17),("12",18),("13",19),("14",20),("15",21),("16",22),("17",23),("18",24),("19",25),("1A",26),("1B",27),("1C",28),("1D",29),("1E",30),("1F",31),("20",32),("21",33),("22",34),("23",35),("24",36),("25",37),("26",38),("27",39),("28",40),("29",41),("2A",42),("2B",43),("2C",44),("2D",45),("2E",46),("2F",47),("30",48),("31",49),("32",50),("33",51),("34",52),("35",53),("36",54),("37",55),("38",56),("39",57),("3A",58),("3B",59),("3C",60),("3D",61),("3E",62),("3F",63),("40",64),("41",65),("42",66),("43",67),("44",68),("45",69),("46",70),("47",71),("48",72),("49",73),("4A",74),("4B",75),("4C",76),("4D",77),("4E",78),("4F",79),("50",80),("51",81),("52",82),("53",83),("54",84),("55",85),("56",86),("57",87),("58",88),("59",89),("5A",90),("5B",91),("5C",92),("5D",93),("5E",94),("5F",95),("60",96),("61",97),("62",98),("63",99),("64",100),("65",101),("66",102),("67",103),("68",104),("69",105),("6A",106),("6B",107),("6C",108),("6D",109),("6E",110),("6F",111),("70",112),("71",113),("72",114),("73",115),("74",116),("75",117),("76",118),("77",119),("78",120),("79",121),("7A",122),("7B",123),("7C",124),("7D",125),("7E",126),("7F",127),("80",128),("81",129),("82",130),("83",131),("84",132),("85",133),("86",134),("87",135),("88",136),("89",137),("8A",138),("8B",139),("8C",140),("8D",141),("8E",142),("8F",143),("90",144),("91",145),("92",146),("93",147),("94",148),("95",149),("96",150),("97",151),("98",152),("99",153),("9A",154),("9B",155),("9C",156),("9D",157),("9E",158),("9F",159),("A0",160),("A1",161),("A2",162),("A3",163),("A4",164),("A5",165),("A6",166),("A7",167),("A8",168),("A9",169),("AA",170),("AB",171),("AC",172),("AD",173),("AE",174),("AF",175),("B0",176),("B1",177),("B2",178),("B3",179),("B4",180),("B5",181),("B6",182),("B7",183),("B8",184),("B9",185),("BA",186),("BB",187),("BC",188),("BD",189),("BE",190),("BF",191),("C0",192),("C1",193),("C2",194),("C3",195),("C4",196),("C5",197),("C6",198),("C7",199),("C8",200),("C9",201),("CA",202),("CB",203),("CC",204),("CD",205),("CE",206),("CF",207),("D0",208),("D1",209),("D2",210),("D3",211),("D4",212),("D5",213),("D6",214),("D7",215),("D8",216),("D9",217),("DA",218),("DB",219),("DC",220),("DD",221),("DE",222),("DF",223),("E0",224),("E1",225),("E2",226),("E3",227),("E4",228),("E5",229),("E6",230),("E7",231),("E8",232),("E9",233),("EA",234),("EB",235),("EC",236),("ED",237),("EE",238),("EF",239),("F0",240),("F1",241),("F2",242),("F3",243),("F4",244),("F5",245),("F6",246),("F7",247),("F8",248),("F9",249),("FA",250),("FB",251),("FC",252),("FD",253),("FE",254),("FF",255),("0a",10),("0b",11),("0c",12),("0d",13),("0e",14),("0f",15),("1a",26),("1b",27),("1c",28),("1d",29),("1e",30),("1f",31),("2a",42),("2b",43),("2c",44),("2d",45),("2e",46),("2f",47),("3a",58),("3b",59),("3c",60),("3d",61),("3e",62),("3f",63),("4a",74),("4b",75),("4c",76),("4d",77),("4e",78),("4f",79),("5a",90),("5b",91),("5c",92),("5d",93),("5e",94),("5f",95),("6a",106),("6b",107),("6c",108),("6d",109),("6e",110),("6f",111),("7a",122),("7b",123),("7c",124),("7d",125),("7e",126),("7f",127),("8a",138),("8b",139),("8c",140),("8d",141),("8e",142),("8f",143),("9a",154),("9b",155),("9c",156),("9d",157),("9e",158),("9f",159),("a0",160),("a1",161),("a2",162),("a3",163),("a4",164),("a5",165),("a6",166),("a7",167),("a8",168),("a9",169),("aa",170),("ab",171),("ac",172),("ad",173),("ae",174),("af",175),("b0",176),("b1",177),("b2",178),("b3",179),("b4",180),("b5",181),("b6",182),("b7",183),("b8",184),("b9",185),("ba",186),("bb",187),("bc",188),("bd",189),("be",190),("bf",191),("c0",192),("c1",193),("c2",194),("c3",195),("c4",196),("c5",197),("c6",198),("c7",199),("c8",200),("c9",201),("ca",202),("cb",203),("cc",204),("cd",205),("ce",206),("cf",207),("d0",208),("d1",209),("d2",210),("d3",211),("d4",212),("d5",213),("d6",214),("d7",215),("d8",216),("d9",217),("da",218),("db",219),("dc",220),("dd",221),("de",222),("df",223),("e0",224),("e1",225),("e2",226),("e3",227),("e4",228),("e5",229),("e6",230),("e7",231),("e8",232),("e9",233),("ea",234),("eb",235),("ec",236),("ed",237),("ee",238),("ef",239),("f0",240),("f1",241),("f2",242),("f3",243),("f4",244),("f5",245),("f6",246),("f7",247),("f8",248),("f9",249),("fa",250),("fb",251),("fc",252),("fd",253),("fe",254),("ff",255)]

!decoderMap = M.fromList dl

