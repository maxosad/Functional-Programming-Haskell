{
module HW3.Parser where
import HW3.Lexer
import HW3.Base
import qualified Data.ByteString as B ( concat)
import Text.Megaparsec.Error (ParseErrorBundle (..))
import Data.Void (Void(..))
import Data.Ratio ((%))
}

%name parse
%tokentype { Token }
%error { parseError }




%token
        '('                             { TOP }
        ')'                             { TCP }
        ','                             { TPiv }
        '#'                             { TSharp    }
        '!'                             { TExcl    }
        
        'add'                           { TAdd }
        'div'                           { TDiv }
        'mul'                           { TMul }
        'sub'                           { TSub }

        cnst                            { TNum  $$}
        byte                            { TByte  $$}
        
        'not'                           { TNot     }
        'and'                           { TAnd     }
        'or'                            { TOr      }
        'less-than'                     { TLT      }
        'greater-than'                  { TGT      }
        'equals'                        { TEq      }
        'not-less-than'                 { TNLT     }
        'not-greater-than'              { TNGT     }
        'not-equals'                    { TNE      }
        'if'                            { TIf      }
        'true'                          { TTrue      }
        'false'                         { TFalse     }
        
        '/'                             { TDel     }
        '+'                             { TPlus     }
        '-'                             { TMinus     }
        '*'                             { TUmn    }
        '<'                             { TL       }
        '>'                             { TG       }
        '>='                            { TGE       }
        '<='                            { TLE       }
        '=='                            { TEQ       }
        '/='                            { TNEQ      }
        '&&'                            { TAND      }
        '||'                            { TOR      }
	
        'length'                        { TLength      }
        'to-upper'                      { TToUpp      }
        'to-lower'                      { TToLow      }
        'reverse'                       { TRevers      }
        'trim'                          { TTrim      }
        'null'                          { TNull      }
    
        text                            { TText $$ }
        'list'                          { TList      }    
        'range'                         { TRange      }    
        'fold'                          { TFold      }    
        '['                             { TOSB     }    
        ']'                             { TCSB     }
		
        numbyte                         { TNumByte $$ } 
        'pack-bytes'                    { TPaB      }   
        'unpack-bytes'                  { TUnB      }    
        'zip'                           { TZip      }    
        'unzip'                         { TUZip      }    
        'encode-utf8'                   { TE8      }    
        'decode-utf8'                   { TD8      }    
        'serialise'                     { TSer      }    
        'deserialise'                   { TDSer      } 


        'read'                          { TRead      }    
        'write'                         { TWrite      }    
        'mkdir'                         { TMkDir      }    
        'cd'                            { TCD      }    
        'cwd'                           { TCWD     }    
			
        'parse-time'                    { TParT     }    
        'now'                           { TNow    }    
		
        'rand'                          { TRand    }    
	    
        'echo'                          { TEcho    }
		
        'count'                         { TCount    }    
        'keys'                          { TKeys    }    
        'values'                        { TValues    }    
        'invert'                        { TInvert    }
		

        '{'                             { TOFB     }    
        '}'                             { TCFB     }
        dot                             { TDot $$ }
        ':'                             { TColon     }  



%right '||'
%right '&&'
%nonassoc '>' '<' '>=' '<=' '==' '/=' 
%left '+' '-'   
%left '*' '/'   
%left NEG  
%%
 
Expr: 
  Expr '||' Expr              { HiExprApply (HiExprValue (HiValueFunction HiFunOr)) [$1, $3] }
  | Expr '&&' Expr            { HiExprApply (HiExprValue (HiValueFunction HiFunAnd)) [$1, $3] }
  | Expr '+' Expr             { HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) [$1, $3] }
  | Expr '-' Expr             { HiExprApply (HiExprValue (HiValueFunction HiFunSub)) [$1, $3] }
  | Expr '>' Expr             { HiExprApply (HiExprValue (HiValueFunction HiFunGreaterThan)) [$1, $3] }
  | Expr '<' Expr             { HiExprApply (HiExprValue (HiValueFunction HiFunLessThan)) [$1, $3] }
  | Expr '>=' Expr            { HiExprApply (HiExprValue (HiValueFunction HiFunNotLessThan)) [$1, $3] }
  | Expr '<=' Expr            { HiExprApply (HiExprValue (HiValueFunction HiFunNotGreaterThan)) [$1, $3] }
  | Expr '==' Expr            { HiExprApply (HiExprValue (HiValueFunction HiFunEquals)) [$1, $3] }
  | Expr '/=' Expr            { HiExprApply (HiExprValue (HiValueFunction HiFunNotEquals)) [$1, $3] }
  | Expr '-' Expr             { HiExprApply (HiExprValue (HiValueFunction HiFunSub)) [$1, $3] }
  | Expr '*' Expr             { HiExprApply (HiExprValue (HiValueFunction HiFunMul)) [$1, $3] }
  | Expr '/' Expr             { HiExprApply (HiExprValue (HiValueFunction HiFunDiv)) [$1, $3] }
  | '-' Expr %prec NEG         { HiExprApply (HiExprValue (HiValueFunction HiFunMul)) [$2, (HiExprValue ( HiValueNumber (-1%1)))] }
  | '(' Expr ')'                 {  $2 }
  | '[' '#' Bytes '#' ']'                 { HiExprValue (HiValueBytes (B.concat $3))   }
  | '[' Args ']'                 {  HiExprApply (HiExprValue (HiValueFunction HiFunList)) $2  }
  | Expr '(' Args ')' {HiExprApply $1 $3    }
  | Expr '!' {HiExprRun $1    }
  | '{' KVArgs '}' {HiExprDict $2   }
  | Value  { HiExprValue $1 }  
  | Expr dot  {HiExprApply $1 [(HiExprValue (HiValueString $2))]    }


KVArgs: 
  Expr ':' Expr ',' KVArgs {($1, $3) : $5}
  | Expr ':' Expr  {[($1, $3)]}
 
  
Args: 
  Expr ',' Args {$1 : $3}
  | Expr {[$1]}

Bytes :
  byte Bytes {$1 : $2}
  | numbyte Bytes {(sTob $1) : $2}
  | 'cd' Bytes {(sTob "cd") : $2}
  | byte {[$1]}
  | numbyte {[sTob $1 ] }
  | 'cd' {[(sTob "cd") ] }
  
Value: 
  cnst {HiValueNumber $1}
  | numbyte  {HiValueNumber (readRational $1)}
  | Fun  {HiValueFunction $1}
  | 'true' { HiValueBool True     }
  | 'false' { HiValueBool False     } 
  | 'null' { HiValueNull     } 
  | text { HiValueString $1 }
  | Actions {$1}

Actions:
  'now' { HiValueAction HiActionNow  }
  | 'cwd' { HiValueAction HiActionCwd }
  
Fun:
  'add'                             { HiFunAdd }
  | 'mul'                           { HiFunMul }
  | 'sub'                           { HiFunSub }
  | 'div'                           { HiFunDiv }
  | 'not'                           { HiFunNot    }
  | 'and'                           { HiFunAnd    }
  | 'or'                            { HiFunOr     }
  | 'less-than'                     { HiFunLessThan     }
  | 'greater-than'                  { HiFunGreaterThan      }
  | 'equals'                        { HiFunEquals      }
  | 'not-less-than'                 { HiFunNotLessThan     }
  | 'not-greater-than'              { HiFunNotGreaterThan     }
  | 'not-equals'                    { HiFunNotEquals      }
  | 'if'                            { HiFunIf      }
  | 'length'                        { HiFunLength      }
  | 'to-upper'                      { HiFunToUpper      }
  | 'to-lower'                      { HiFunToLower      }
  | 'reverse'                       { HiFunReverse      }
  | 'trim'                          { HiFunTrim      }
  | 'list'                          { HiFunList      }
  | 'range'                         { HiFunRange      }
  | 'fold'                          { HiFunFold      }
  | 'pack-bytes'                    { HiFunPackBytes      }    
  | 'unpack-bytes'                  { HiFunUnpackBytes      }    
  | 'zip'                           { HiFunZip      }    
  | 'unzip'                         { HiFunUnzip      }    
  | 'encode-utf8'                   { HiFunEncodeUtf8      }    
  | 'decode-utf8'                   { HiFunDecodeUtf8      }    
  | 'serialise'                     { HiFunSerialise      }    
  | 'deserialise'                   { HiFunDeserialise      }  
  | 'read'                          { HiFunRead      }    
  | 'write'                         { HiFunWrite      }    
  | 'mkdir'                         { HiFunMkDir      }    
  | 'cd'                            { HiFunChDir      }    
  | 'parse-time'                    { HiFunParseTime      }    
  | 'rand'                          { HiFunRand      }  
  | 'echo'                          { HiFunEcho    }      
  | 'count'                         { HiFunCount    }      
  | 'keys'                          { HiFunKeys    }      
  | 'values'                        { HiFunValues    }      
  | 'invert'                        { HiFunInvert    }      
  

{









parseError :: [Token] -> a
parseError _ = error "parse error"

}