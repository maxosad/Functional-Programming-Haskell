{
module HW3.Lexer where    
import Data.Ratio ( (%) )
import Data.Text (Text (..), pack) 
import qualified Data.ByteString as B (ByteString (..))
import Data.Text.Encoding (encodeUtf8)
import Data.List (init, tail)
import Sound.OSC.Coding.Byte (encode_u8, decode_u8)
import Data.Char (digitToInt)
import qualified Data.ByteString.Lazy as BL (fromStrict, toStrict)
}

%wrapper "basic"

$digit=0-9
$alpha=[a-zA-Z]
$alphadigit=[0-9a-zA-Z]
$byte=[a-f0-9]
$anybq=[^"]

tokens :-
    $white                        ;
   
 
    \(                            { \s -> TOP     }    
    \)                            { \s -> TCP     }
    \,                            { \s -> TPiv    }
    \#                            { \s -> TSharp    }
    \!                            { \s -> TExcl    }
	
    add                           { \s -> TAdd    }
    mul                           { \s -> TMul    }
    div                           { \s -> TDiv    }
    sub                           { \s -> TSub    }
	
    not                           { \s -> TNot    }
    and                           { \s -> TAnd    }
    or                            { \s -> TOr     }
    less\-than                    { \s -> TLT     }
    greater\-than                 { \s -> TGT     }
    equals                        { \s -> TEq    }
    not\-less\-than               { \s -> TNLT     }
    not\-greater\-than            { \s -> TNGT     }
    not\-equals                   { \s -> TNE     }
    if                            { \s -> TIf     }
    true                          { \s -> TTrue     }
    false                         { \s -> TFalse     }
   
    
    \/	                          { \s -> TDel     }
    \+	                          { \s -> TPlus     }
    \-	                          { \s -> TMinus     }
    \*	                          { \s -> TUmn    }
    \<                            { \s -> TL       }
    \>                            { \s -> TG       }
    ">="                           { \s -> TGE       }
    "<="                           { \s -> TLE       }
    "=="                           { \s -> TEQ       }
    "/="                           { \s -> TNEQ      }
    "&&"                           { \s -> TAND      }
    "||"                          { \s -> TOR      }
	
    "length"                          { \s -> TLength      }
    "to-upper"                          { \s -> TToUpp      }
    "to-lower"                          { \s -> TToLow      }
    "reverse"                          { \s -> TRevers      }
    "trim"                          { \s -> TTrim      }
    "null"                         { \s -> TNull      }
	
    "list"                          { \s -> TList      }    
    "range"                          { \s -> TRange      }    
    "fold"                          { \s -> TFold      }    
    "["                            { \s -> TOSB     }    
    "]"                            { \s -> TCSB     }   
    
    
	
	
    "pack-bytes"                          { \s -> TPaB      }    
    "unpack-bytes"                          { \s -> TUnB      }    
    "zip"                          { \s -> TZip      }    
    "unzip"                          { \s -> TUZip      }    
    "encode-utf8"                          { \s -> TE8      }    
    "decode-utf8"                          { \s -> TD8      }    
    "serialise"                          { \s -> TSer      }    
    "deserialise"                          { \s -> TDSer      }    
  
	
	
	
    "read"                          { \s -> TRead      }    
    "write"                          { \s -> TWrite      }    
    "mkdir"                          { \s -> TMkDir      }    
    "cd"                          { \s -> TCD      }    
    "cwd"                          { \s -> TCWD     }    
	
	

    "parse-time"                          { \s -> TParT     }    
    "now"                          { \s -> TNow    }    

	
    "rand"                          { \s -> TRand    }    
	
    "echo"                          { \s -> TEcho    }    
	
	
	
    "count"                        { \s -> TCount    }    
    "keys"                         { \s -> TKeys    }    
    "values"                       { \s -> TValues    }    
    "invert"                       { \s -> TInvert    }  

	    
    "{"                            { \s -> TOFB     }    
    "}"                            { \s -> TCFB     }  
    ":"                            { \s -> TColon     }  
	
    \.$alpha$alphadigit*(\-$alphadigit+)*     	{ \s -> TDot (pack (tail s))   }  
	
	
    $digit$digit                   { \s -> TNumByte s }
    $digit+                        { \s -> TNum  (readRational s) }
    $digit+\.$digit+               { \s -> TNum  (readRational s) }
    $digit+\.$digit+e$digit+       { \s -> TNum  (readRational s) }
    $digit+e$digit+                { \s -> TNum  (readRational s) }
    $byte$byte                     { \s -> TByte (sTob s)     }

    \"$anybq*\"                    { \s -> TText (pack (init (tail s)))     }
{


helper :: String -> String
helper str = if null str then "0" else tail str

sTob :: String -> B.ByteString
sTob s =  (BL.toStrict  (encode_u8  ((digitToInt (head s) ) * 16 + (digitToInt (last s) ))))


readRational :: String -> Rational
readRational input = (read intPart % 1 + read fracPart % (10 ^ length fracPart)) * (10 ^ read e)
  where (num, expon) = span (/='e') input
        (intPart, fromDot) = span (/='.') num
        fracPart           = helper fromDot
        e                  = helper expon






data Token = TOP
           | TCP
           | TPiv
           | TAdd
           | TDiv
           | TMul
           | TSub
           | TVar String
           | TNum Rational 
           | TNot
           | TAnd
           | TOr
           | TLT
           | TGT
           | TEq
           | TNLT
           | TNGT
           | TNE
           | TIf
           | TTrue
           | TFalse
           | TDel
           | TPlus
           | TMinus
           | TUmn
           | TL
           | TG
           | TGE
           | TLE
           | TEQ
           | TNEQ
           | TAND
           | TOR
           | TLength
           | TToUpp
           | TToLow
           | TRevers
           | TTrim
           | TNull
           | TText Text
           | TByte B.ByteString
           | TList
           | TRange
           | TFold
           | TOSB
           | TCSB
           | TPaB
           | TUnB
           | TZip
           | TUZip
           | TE8
           | TD8
           | TSer
           | TDSer
           | TSharp
           | TExcl
           | TRead
           | TWrite
           | TMkDir
           | TCD
           | TCWD
           | TParT
           | TNow
           | TRand
           | TEcho
           | TCount
           | TKeys
           | TValues
           | TInvert
           | TDot Text
           | TOFB
           | TCFB
           | TColon  
           | TNumByte String
           deriving (Eq, Show)
}

	














	
	
	
	
	
	
	
	
	
	
	
	