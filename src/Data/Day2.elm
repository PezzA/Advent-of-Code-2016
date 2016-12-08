module Data.Day2 exposing(..)

import Common exposing (TestRecord)

day2Data : List TestRecord
day2Data =
    [ ( "Test One", "ULL\x0D\nRRDDD\x0D\nLURDL\x0D\nUUUUD" )
    , ( "Test Two", "RUDULRLLUULRURDDRRUDURULLLDRLRLUDDLUDUDDUDRRDUDULDUUULLRULLRLDDLDLDDRLRRRRUDLLDDUULDRLLUDDRRUURLULRRRDLLURRUUDURUDDURLUDDDLUDDUUDUURUDLRDRDRLRDRLDRUDRUUDLRDDRRURDDLRDDRRURDUDDLULLUDRURURRRLRRUDUULULULRRLDLUDUURRLLRUDLLDRDDLRRRULRUDLULDDLLLULDLRUDLLLLRDDLRDRLDRLLRDRRDLRDULULRLLLDRUDRRRUULRUULDRURLUDRURRDLLDLRDLDDDDRRLUDLRRLUUUURDRDDLRRURURRDUULLRLURLURUDDDRDURDUUDRLRLRRLDDLDLDLDDDUDDULURLDDLLRLRRDULUDDLULRLUDDLDLRULUUUDRLDRUDURLUDDRLLRUULDLRRRRDLLLLURULLRDRRUDLUULRRDLLRLRLUDLDDULLDLLRDLDLL\x0D\nLLUUUUUUDUDRLRDRDLDURRRLLRRLRURLLUURRLLUDUDLULUURUUURDLUDLDDLULLRDLRUULDLRDUDURLLDDUDUDULLUDDUULLLUULRRRLULRURRDLRUDUDDURRRDRUURDURLLULLRULLDRUULLURLDRDUUDDDDDDRRLDRLRRRLULDDUURRLLLLDRURLURDRDRDURUDUURRDUDUDRLLUUDDRLUDDDRDLDLRLDRURRDLLRULDRLLURURRLUULLRLRRURDDRDRUUURUURUUUDLLRRLUDRLDLRLURLDLUDDUDDDLDUDRRLDLRURULRLLRDUULURRRULDLLLRLDDDUURRRRDULLRURRLULULDLRRUDUDDLRUURDLDUDDUDRRDLRRRDUDUUUDLLDDDDLURLURRRUUULLLULRRLLLLLLULDUUDLRUDRRDLRDUUDUDLLRLDLLRUURDUUURUUUDDLLUUDLULDURLULULUUUDRUDULLURRULRULLRDLDDU\x0D\nRLUUURULLDLRLDUDRDURRDUURLLUDDDUULRRRLRLURDDRUULUDULDUUDDDDUDDDDRUDDLDUUDRUDLRRRLLRDDLLLRLLRUULRUULDDRURRLURRLRLULDDRRRDDURDDRDRDULRUDRUUDULRLLULDLRLLDRULRDDRRDDUDLRLLUDRDRRRLUDULRDLRDDURRUUDDRRUDURRUUUDDRRDUDURLUUDUDUURDDDLURLULLUULULURUDUUDRUDULLUUULURDLDUULLDDLLDULRLRLRDUUURUUDLRLDURUDRLDULLUDLDLLRDUURRDUDURLUUUDLLRRULRLULRLDLLURDURRULRLLRRDUDLLRDRRRRDLUUDRUUUDDLRLUDDDDDDRURRRUUURRDLLRURLDDLLDLRRLLLDRRULRRUDLDRDDRRLULURLLUURURURRRRUUUUURUDURLRLLLULULDLLDLRDRRULUDUDRDRRDRDRRDUDLLLRUDRUDDDULRULRRRDRLRUUUURUDURDUUULLULRUDDULDUUDLDURRD\x0D\nULRULDDLDLULLLRRRLRUDDDDDLLDDUDLRRDULUUDRDLRRURDRRLUULRURUDRRULDLLLUDRUUDULULUDDRUDDDRDURRRDRDUUURLRDULUDRDRLDRUDDLLLDRRULUDLUDLDLLRRUDUULULDLDLLUURDLDDLLUUDURLURLLLDRDLDRRLRULUURRDRULRUUURULRRUDDDDLLDLDDLLRRLRRRRDUUDUDLDRDRRURDLRURULDLRDLLLLRUDRLLRDLRLRDURDRUDURRRLRDRDLLRLUDDDDRLRLLDUURRURLUURUULUDLUURDRRUDDLUDUDDDURRDRUDRLRULDULUUUUUUDDUDRUDUUURUDRRDLUDLUUDUULUDURDLDDDLLURRURUUDUDDRRDRLLULULDRLRURRDDDRDUUURDDDRULUDRDDLDURRLDDDLRRRLDDRDURULDLUDLLLURLURRLRRULDLLDDUDRRULDRRRRLURRUULRRRUDLURDLLDLLDULUUDRRLDLLLDRLRUDLUULDLDRUDUDURDRUDRDDDLRLULLUR\x0D\nLRLUUURRLRRRRRUURRLLULRLULLDLUDLUDRDDRLDLRLULLURDURLURDLLRLDUUDDURRRRLDLLRULLRLDLLUUDRLDDLLDRULDRLLRURDLRURRUDLULLRURDLURRURUDULLDRLLUUULUDRURRUUDUDULUUULRLDDULDRDLUDDUDDDLRURULLDLLLRLLUURDLRUDLLLLDLLRLRUUUDDRUUUUDLDLRDDURLDURUULLLUUDLLLLDULRRRLLDLDRRDRLUDRUDURLLUDLRLLUDUDRDDDRDLRDLRULUULDRLUDLRLDUURLRRLUDDDUUDDDUDRLDLDUDLURUULLDDDURUUULRLUDLDURUUDRDRURUDDUURDUUUDLLDLDLDURUURLLLLRURUURURULRULLRUDLRRUUUUUDRRLLRDDUURDRDRDDDUDRLURDRRRUDLLLDURDLUUDLLUDDULUUDLDUUULLDRDLRURUURRDURRDLURRRRLLUUULRDULDDLDUURRDLDLLULRRLLUDLDUDLUUL" )
    ]
