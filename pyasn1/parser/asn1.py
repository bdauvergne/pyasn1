# -*- coding: utf-8 -*-
from pymeta.grammar import OMeta
import pymeta.runtime


__all__ = ('Asn1Grammar1988',)

asn1_1988_grammar = '''
comma = ourtoken(",")
lpar = ourtoken("(")
rpar = ourtoken(")")
lbracket = ourtoken("{")
rbracket = ourtoken("}")
semi_colon = ourtoken(";")
equal = ourtoken("::=")
ellipsis2 = ourtoken("..")
ellipsis3 = ourtoken("...")

no_double_colon = '-' ~('-')
no_colon = ~('-') ~('\\n') anything
comment_content = no_colon | no_double_colon
end_comment = "--" | '\\n'
comment = token("--") comment_content* end_comment
blank = spaces comment* spaces

ourtoken :x = blank match_string(x)

identifier = blank letter:a ?(a.islower()) (letterOrDigit | "-")*
typereference = blank letter:a ?(a.isupper()) (letterOrDigit | "-")*:x ?(not reserved(a + ''.join(x)))
valuereference = identifier
modulereference = typereference


exp_prefix = 'E' | 'e'
exp = exp_prefix relative_integer
fract = "." integer
float =  integer:i fract?:f exp?:e ?(f or e)
sign = '+' | '-'
relative_integer = sign? integer
integer = blank digit+
number = float | integer

named_constant = identifier ourtoken("(") number ourtoken(")")
enum = spaces (number|named_constant|identifier)
oid = lbracket enum+ rbracket

implicit_or_explicit = ourtoken("IMPLICIT") | ourtoken("EXPLICIT")
tag_policies = ourtoken("AUTOMATIC") | implicit_or_explicit
tag_policy = tag_policies ourtoken("TAGS")

bstring_lex = '0' | '1'
bstring_content = bstring_lex*
bstring = ourtoken("'") bstring_content "'B"

hstring_lex = digit|'A'|'B'|'C'|'D'|'E'|'F'
hstring_content = hstring_lex*
hstring = ourtoken("'") hstring_content "'H"

seq_type_expression = identifier:name type_expression:type -> dict(name=name, type=type)
seq_type_expressions = seq_type_expression (ourtoken(",") seq_type_expression)*

class = ourtoken("UNIVERSAL")
    | ourtoken("APPLICATION")
    | ourtoken("CONTEXT")
    | ourtoken("PRIVATE")

tag = ourtoken("[") class? blank integer ourtoken("]") implicit_or_explicit?

any_type = ourtoken("ANY") | ourtoken("ANY") ourtoken("DEFINED") ourtoken("BY") identifier

value = boolean_value
    | real_value
    | integer_value
    | bitstring_value
    | octetstring_value
    | null_value
    | object_identifier_value
    | string_value
    | sequence_value
    | sequence_of_value
    | set_value
    | set_of_value
    | choice_value
    | selection_value

boolean_type = ourtoken("BOOLEAN")
boolean_value = ourtoken("TRUE") | ourtoken("FALSE")

null_type = ourtoken('NULL')
null_value = null_type

integer_type = ourtoken("INTEGER") named_number_list2?
named_number_list2 = lbracket named_number_list rbracket
named_number_list = named_number comma named_number_list
    | named_number

named_number = identifier lpar relative_integer rpar
signed_number = relative_integer
integer_value = signed_number | identifier

enumerated_type = ourtoken("ENUMERATED") named_number_list2?
enumerated_value = identifier

real_type = ourtoken("REAL")
real_value = numeric_real_value | special_real_value
numeric_real_value = 
    lbracket mantissa comma base comma exponent rbracket
    | float
mantissa = signed_number
base = ourtoken("2") | ourtoken("10")
exponent = signed_number
special_real_value = ourtoken("PLUS-INFINITY") | ourtoken("MINUS-INFINITY")

bitstring_type = ourtoken("BIT") ourtoken("STRING") named_number_list2?
bitstring_value = bstring | hstring | lbracket identifier_list rbracket
identifier_list = identifier identifier_list | identifier

octetstring_type = ourtoken("OCTET") ourtoken("STRING")
octetstring_value = bstring | hstring


element_type_list = element_type comma element_type_list
    | element_type

value_list = value comma value_list
    | value

named_value_list = named_value comma named_value_list
    | named_value

named_value = identifier value | value

sequence_value = lbracket named_value_list? rbracket
sequence_of_value = lbracket value_list? rbracket

set_of_value = sequence_of_value
set_value = sequence_value

element_type = named_type ourtoken("OPTIONAL")
    | named_type ourtoken("DEFAULT") value
    | ourtoken("COMPONENTS") ourtoken("OK") type
    | named_type

sequence_type = ourtoken("SEQUENCE") ourtoken("{") ourtoken("}")
    | ourtoken("SEQUENCE") ourtoken("{") element_type_list ourtoken("}")

sequence_of_type = ourtoken("SEQUENCE OF") type

set_type = ourtoken("SET") ourtoken("{") ourtoken("}")
    | ourtoken("SET") ourtoken("{") element_type_list ourtoken("}")

set_of_type = ourtoken("SET") ourtoken("OF") type

alternative_type_list = named_type comma alternative_type_list | named_type
choice_type = ourtoken("CHOICE") lbracket alternative_type_list rbracket

choice_value = named_value

selection_type = identifier ourtoken("<") type
selection_value = named_value

tagged_value = value

object_identifier_type = ourtoken("OBJECT") ourtoken("IDENTIFIER") 

defined_value = modulereference valuereference | valuereference

object_identifier_value = 
    lbracket defined_value objid_component_list rbracket
    | lbracket objid_component_list rbracket

objid_component_list = 
      objid_component objid_component_list
    | objid_component

objid_component = name_and_number_form | identifier | integer

name_form = identifier
number_form = integer | defined_value
name_and_number_form = identifier lpar integer rpar

tagged_type = tag type

builtin_type = boolean_type 
    | integer_type 
    | enumerated_type 
    | real_type 
    | bitstring_type 
    | octetstring_type
    | sequence_type
    | sequence_of_type
    | set_type
    | set_of_type
    | choice_type
    | any_type
    | tagged_type
    | object_identifier_type
    | enumerated_type
    | real_type
    | string_type
    | useful_type
    | null_type

useful_type = typereference

string_type_prefix = "Numeric" 
    | "Printable" 
    | "Teletex" 
    | "Videotex" 
    | "Visible" 
    | "Graphic"
    | "General"
    | "UTF8"
    | "T61"
    | "IA5"

string_type = blank string_type_prefix "String"
string_value = ourtoken("\\"") (~('"') anything)* '"'

named_type = identifier type | type

defined_type = modulereference typereference | typereference

subtype_value_set = 
      contained_subtype
    | value_range
    | permitted_alphabet
    | size_constraint
    | inner_type_constraints
    | single_value

single_value = value

contained_subtype = ourtoken("INCLUDES") type

value_range = lower_endpoint ellipsis2 upper_endpoint
lower_end_value = ourtoken("MIN") | value
lower_endpoint = lower_end_value ourtoken("<") | lower_end_value
upper_end_value = ourtoken("MAX") | value
upper_endpoint = ourtoken("<") upper_end_value | upper_end_value

size_constraint = ourtoken("SIZE") subtype_spec

permitted_alphabet = ourtoken("FROM") subtype_spec

inner_type_constraints =
      ourtoken("WITH") ourtoken("COMPONENTS") multiple_type_constraints
    | ourtoken("WITH") ourtoken("COMPONENT") single_type_constraint

single_type_constraint = subtype_spec

multiple_type_constraints = partial_specification | full_specification

full_specification = lbracket type_constraints rbracket

partial_specification = lbracket ellipsis3 comma type_constraints rbracket

type_constraints = named_constraint comma type_constraints
    | named_constraint

named_constraint = identifier constraint
    | constraint

constraint = value_constraint? presence_constraint?
value_constraint = subtype_spec
presence_constraint = ourtoken("PRESENT") | ourtoken("ABSENT") | ourtoken("OPTIONAL")


subtype_value_set_list = subtype_value_set ourtoken("|") subtype_value_set_list
    | subtype_value_set

subtype_spec = ourtoken("(") subtype_value_set_list ourtoken(")") 

type = 
      ourtoken("SET") size_constraint ourtoken("OF") type
    | ourtoken("SEQUENCE") size_constraint ourtoken("OF") type
    | type subtype_spec
    | builtin_type
    | defined_type

type_assignment = typereference equal type

value_assignment = valuereference type equal value

assignment_list = (type_assignment|value_assignment)*

module_identifier = modulereference object_identifier_value?

symbols_from_module_list = symbols_from_module symbols_from_module_list
    | symbols_from_module

symbols_from_module = symbol_list ourtoken("FROM") module_identifier


symbol_list = symbol comma symbol_list
    | symbol

symbol = typereference | valuereference

symbols_exported = symbols_from_module_list
symbols_imported = symbols_from_module_list

exports = ourtoken("EXPORTS") symbols_exported semi_colon
imports = ourtoken("IMPORTS") symbols_imported semi_colon

grammar =
    module_identifier
    ourtoken("DEFINITIONS") 
    tag_policy? 
    equal
    ourtoken("BEGIN") 
    exports?
    imports?
    assignment_list
    ourtoken("END")
'''

reserved_words = ('BOOLEAN', 'BEGIN', 'INTEGER', 'END', 'BIT', 'DEFINITIONS',
    'STRING', 'EXPLICIT', 'OCTET', 'ENUMERATED', 'NULL', 'EXPORTS', 'SEQUENCE',
    'IMPORTS', 'OF', 'REAL', 'SET', 'INCLUDES', 'IMPLICIT', 'MIN', 'CHOICE', 'MAX',
    'ANY', 'SIZE', 'EXTERNAL', 'FROM', 'OBJECT', 'WITH', 'IDENTIFIER', 'COMPONENT',
    'OPTIONAL', 'PRESENT', 'DEFAULT', 'ABSENT', 'COMPONENTS', 'DEFINED',
    'UNIVERSAL', 'BY', 'APPLICATION', 'PLUS-INFINITY', 'PRIVATE', 'MINUS-INFINITY',
    'TRUE', 'TAGS', 'FALSE' )

def reserved(word):
    return word in reserved_words

Asn1Grammar1988 = OMeta.makeGrammar(asn1_grammar, globals(),'Asn1Grammar1988')

if __name__ == '__main__':
    import pprint
    y = '''
MY-MODULE1 { iso standard 2345 modules (0) basic-types (1) }
DEFINITIONS
AUTOMATIC TAGS ::=
BEGIN

-- EXPORTS ALL --
IMPORTS perBasicAligned FROM ASN1-Object-Identifier-Module
    { joint-iso-itu-t asn1(1) specification(0) modules(0)
object-identifiers(1) };


My-sequence ::= SEQUENCE {
        first   BOOLEAN,
        second  INTEGER OPTIONAL,
        third   INTEGER DEFAULT 129,
        fourth  BOOLEAN DEFAULT TRUE,
        fifth   REAL    DEFAULT 0.629,
          -- Or DEFAULT 62.9E-2
        sixth   UTF8String DEFAULT "",
          -- Unicode characters
        seventh IA5String  DEFAULT "James Morrison",
          -- ASCII characters
        eighth  BIT STRING   DEFAULT '101100011'B,
        ninth   OCTET STRING DEFAULT '89AEF764'H,
        tenth   Alternatives }

Alternatives ::= CHOICE {
        first-alternative       TypeA,
        second-alternative      TypeB,
        third-alternative       NULL  }

DailyMaxTemperaturesForMonth ::=
       SEQUENCE SIZE(28..31) OF INTEGER

VersionsSupported ::= BIT STRING {
        version1 (0),
        version2 (1),
        version3 (2) }

Message ::= SEQUENCE {
        message-id      INTEGER,
        version-bit-map  VersionsSupported
                 DEFAULT {version1} }

Color ::= INTEGER {
        red(10), orange(20), yellow(30), green(40),
        blue(50), indigo(60), violet(70) } (0..80)

Codes ::= ENUMERATED {code1(0),code2(1),code3(2)}

oid1 OBJECT IDENTIFIER ::=
 {iso standard 2345 modules (0) basic-types (1)}

oid2 OBJECT IDENTIFIER ::= {joint-iso-itu-t ds(5)}

oid3 OBJECT IDENTIFIER ::= { oid2 modules(0) }

oid4 OBJECT IDENTIFIER ::= { oid3 basic-types(1) }

oid5 OBJECT IDENTIFIER ::= { 2 5 0 1 } -- equals oid4

TypeA ::= INTEGER (0..MAX)             -- only non-negative values

TypeB ::= INTEGER (-6..3 | 10..30)     -- only -6 to 3 or 10-30

TypeD ::= SEQUENCE SIZE (0..10) OF INTEGER

TypeE ::= IA5String (SIZE (1..25))(FROM ("A" .. "Z"))
        -- Only sizes 1-25 and characters "A"-"Z" allowed

TypeI ::= SEQUENCE {a  INTEGER OPTIONAL,
                 b  INTEGER OPTIONAL,
                 c  INTEGER OPTIONAL }
                    (WITH COMPONENTS {... , a PRESENT } |
                     WITH COMPONENTS {... , b PRESENT } |
                     WITH COMPONENTS {... , c PRESENT } )
                     -- Note the use of ellipsis in the WITH COMPONENTS
                     -- syntax.  This is, however, NOT the extension
                     -- ellipsis, but is a use of the same three dots
                     -- in a way that is specific to WITH COMPONENTS.

Codes2 ::= ENUMERATED {code1(1), code2(2), v2-code(3), another-code(4) }


END
'''
    x = Asn1Grammar(y)
    try:
        pprint.pprint( x.apply('grammar'))
    except pymeta.runtime._MaybeParseError, e:
        pos, error = e.args
        i = y.rfind('\n', 0, pos) + 1
        j = y.find('\n', pos)
        if j == -1:
            j = len(y)
        print 'Error:', error
        print y[i:j]
        print y[pos:pos+5]
        print ' ' * max(0, pos-i-1), '^'

