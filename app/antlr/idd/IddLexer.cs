//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     ANTLR Version: 4.13.1
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

// Generated from Idd.g4 by ANTLR 4.13.1

// Unreachable code detected
#pragma warning disable 0162
// The variable '...' is assigned but its value is never used
#pragma warning disable 0219
// Missing XML comment for publicly visible type or member '...'
#pragma warning disable 1591
// Ambiguous reference in cref attribute
#pragma warning disable 419

using System;
using System.IO;
using System.Text;
using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Misc;
using DFA = Antlr4.Runtime.Dfa.DFA;

[System.CodeDom.Compiler.GeneratedCode("ANTLR", "4.13.1")]
[System.CLSCompliant(false)]
public partial class IddLexer : Lexer {
	protected static DFA[] decisionToDFA;
	protected static PredictionContextCache sharedContextCache = new PredictionContextCache();
	public const int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, FIELD_TYPE=8, 
		AUTOCALCULATABLE_STATEMENT=9, AUTOSIZABLE_STATEMENT=10, BEGIN_EXTENSIBLE_STATEMENT=11, 
		DEFAULT_STATEMENT=12, DEPRECATED_STATEMENT=13, EXTENSIBLE_STATEMENT=14, 
		EXTERNAL_LIST_STATEMENT=15, FIELD_STATEMENT=16, FORMAT_STATEMENT=17, GROUP_STATEMENT=18, 
		IP_UNITS_STATEMENT=19, KEY_STATEMENT=20, MEMO_STATEMENT=21, NOTE_STATEMENT=22, 
		OBJECT_LIST_STATEMENT=23, OBSOLETE_STATEMENT=24, REFERENCE_STATEMENT=25, 
		REFERENCE_CLASS_NAME_STATEMENT=26, REQUIRED_FIELD_STATEMENT=27, REQUIRED_OBJECT_STATEMENT=28, 
		RETAINCASE_STATEMENT=29, UNIQUE_OBJECT_STATEMENT=30, UNITS_STATEMENT=31, 
		INTEGER=32, REALNUMBER=33, ALPHA_OPTION=34, NUMERIC_OPTION=35, OBJECT_NAME=36, 
		COMMENT=37, FIELD_SEPARATOR=38, OBJECT_TERMINATOR=39, WS=40;
	public static string[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static string[] modeNames = {
		"DEFAULT_MODE"
	};

	public static readonly string[] ruleNames = {
		"T__0", "T__1", "T__2", "T__3", "T__4", "T__5", "T__6", "FIELD_TYPE", 
		"AUTOCALCULATABLE_STATEMENT", "AUTOSIZABLE_STATEMENT", "BEGIN_EXTENSIBLE_STATEMENT", 
		"DEFAULT_STATEMENT", "DEPRECATED_STATEMENT", "EXTENSIBLE_STATEMENT", "EXTERNAL_LIST_STATEMENT", 
		"FIELD_STATEMENT", "FORMAT_STATEMENT", "GROUP_STATEMENT", "IP_UNITS_STATEMENT", 
		"KEY_STATEMENT", "MEMO_STATEMENT", "NOTE_STATEMENT", "OBJECT_LIST_STATEMENT", 
		"OBSOLETE_STATEMENT", "REFERENCE_STATEMENT", "REFERENCE_CLASS_NAME_STATEMENT", 
		"REQUIRED_FIELD_STATEMENT", "REQUIRED_OBJECT_STATEMENT", "RETAINCASE_STATEMENT", 
		"UNIQUE_OBJECT_STATEMENT", "UNITS_STATEMENT", "INTEGER", "REALNUMBER", 
		"ALPHA_OPTION", "NUMERIC_OPTION", "OBJECT_NAME", "COMMENT", "FIELD_SEPARATOR", 
		"OBJECT_TERMINATOR", "WS", "NEWLINE"
	};


	public IddLexer(ICharStream input)
	: this(input, Console.Out, Console.Error) { }

	public IddLexer(ICharStream input, TextWriter output, TextWriter errorOutput)
	: base(input, output, errorOutput)
	{
		Interpreter = new LexerATNSimulator(this, _ATN, decisionToDFA, sharedContextCache);
	}

	private static readonly string[] _LiteralNames = {
		null, "'\\maximum'", "'<'", "'\\min-fields '", "'\\minimum'", "'>'", "'\\type '", 
		"'\\unitsBasedOnField '", null, null, null, "'\\begin-extensible'", null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, "','", "';'"
	};
	private static readonly string[] _SymbolicNames = {
		null, null, null, null, null, null, null, null, "FIELD_TYPE", "AUTOCALCULATABLE_STATEMENT", 
		"AUTOSIZABLE_STATEMENT", "BEGIN_EXTENSIBLE_STATEMENT", "DEFAULT_STATEMENT", 
		"DEPRECATED_STATEMENT", "EXTENSIBLE_STATEMENT", "EXTERNAL_LIST_STATEMENT", 
		"FIELD_STATEMENT", "FORMAT_STATEMENT", "GROUP_STATEMENT", "IP_UNITS_STATEMENT", 
		"KEY_STATEMENT", "MEMO_STATEMENT", "NOTE_STATEMENT", "OBJECT_LIST_STATEMENT", 
		"OBSOLETE_STATEMENT", "REFERENCE_STATEMENT", "REFERENCE_CLASS_NAME_STATEMENT", 
		"REQUIRED_FIELD_STATEMENT", "REQUIRED_OBJECT_STATEMENT", "RETAINCASE_STATEMENT", 
		"UNIQUE_OBJECT_STATEMENT", "UNITS_STATEMENT", "INTEGER", "REALNUMBER", 
		"ALPHA_OPTION", "NUMERIC_OPTION", "OBJECT_NAME", "COMMENT", "FIELD_SEPARATOR", 
		"OBJECT_TERMINATOR", "WS"
	};
	public static readonly IVocabulary DefaultVocabulary = new Vocabulary(_LiteralNames, _SymbolicNames);

	[NotNull]
	public override IVocabulary Vocabulary
	{
		get
		{
			return DefaultVocabulary;
		}
	}

	public override string GrammarFileName { get { return "Idd.g4"; } }

	public override string[] RuleNames { get { return ruleNames; } }

	public override string[] ChannelNames { get { return channelNames; } }

	public override string[] ModeNames { get { return modeNames; } }

	public override int[] SerializedAtn { get { return _serializedATN; } }

	static IddLexer() {
		decisionToDFA = new DFA[_ATN.NumberOfDecisions];
		for (int i = 0; i < _ATN.NumberOfDecisions; i++) {
			decisionToDFA[i] = new DFA(_ATN.GetDecisionState(i), i);
		}
	}
	private static int[] _serializedATN = {
		4,0,40,720,6,-1,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,7,
		6,2,7,7,7,2,8,7,8,2,9,7,9,2,10,7,10,2,11,7,11,2,12,7,12,2,13,7,13,2,14,
		7,14,2,15,7,15,2,16,7,16,2,17,7,17,2,18,7,18,2,19,7,19,2,20,7,20,2,21,
		7,21,2,22,7,22,2,23,7,23,2,24,7,24,2,25,7,25,2,26,7,26,2,27,7,27,2,28,
		7,28,2,29,7,29,2,30,7,30,2,31,7,31,2,32,7,32,2,33,7,33,2,34,7,34,2,35,
		7,35,2,36,7,36,2,37,7,37,2,38,7,38,2,39,7,39,2,40,7,40,1,0,1,0,1,0,1,0,
		1,0,1,0,1,0,1,0,1,0,1,1,1,1,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,
		2,1,2,1,2,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,4,1,4,1,5,1,5,1,5,1,5,
		1,5,1,5,1,5,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,
		6,1,6,1,6,1,6,1,6,1,6,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,
		1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,
		7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,
		1,7,1,7,1,7,3,7,196,8,7,1,8,1,8,1,8,1,8,1,8,1,8,1,8,1,8,1,8,1,8,1,8,1,
		8,1,8,1,8,1,8,1,8,1,8,1,8,1,8,1,8,1,9,1,9,1,9,1,9,1,9,1,9,1,9,1,9,1,9,
		1,9,1,9,1,9,1,9,1,9,1,9,1,10,1,10,1,10,1,10,1,10,1,10,1,10,1,10,1,10,1,
		10,1,10,1,10,1,10,1,10,1,10,1,10,1,10,1,10,1,11,1,11,1,11,1,11,1,11,1,
		11,1,11,1,11,1,11,1,11,1,11,5,11,262,8,11,10,11,12,11,265,9,11,1,11,1,
		11,1,12,1,12,1,12,1,12,1,12,1,12,1,12,1,12,1,12,1,12,1,12,1,12,1,12,1,
		12,1,13,1,13,1,13,1,13,1,13,1,13,1,13,1,13,1,13,1,13,1,13,1,13,1,13,5,
		13,296,8,13,10,13,12,13,299,9,13,1,13,1,13,1,14,1,14,1,14,1,14,1,14,1,
		14,1,14,1,14,1,14,1,14,1,14,1,14,1,14,1,14,1,14,1,14,1,14,5,14,320,8,14,
		10,14,12,14,323,9,14,1,14,1,14,1,15,1,15,1,15,1,15,1,15,1,15,1,15,1,15,
		1,15,5,15,336,8,15,10,15,12,15,339,9,15,1,15,1,15,1,16,1,16,1,16,1,16,
		1,16,1,16,1,16,1,16,1,16,1,16,5,16,353,8,16,10,16,12,16,356,9,16,1,16,
		1,16,1,17,1,17,1,17,1,17,1,17,1,17,1,17,1,17,1,17,5,17,369,8,17,10,17,
		12,17,372,9,17,1,17,1,17,1,18,1,18,1,18,1,18,1,18,1,18,1,18,1,18,1,18,
		1,18,1,18,1,18,5,18,388,8,18,10,18,12,18,391,9,18,1,18,1,18,1,19,1,19,
		1,19,1,19,1,19,1,19,1,19,5,19,402,8,19,10,19,12,19,405,9,19,1,19,1,19,
		1,20,1,20,1,20,1,20,1,20,1,20,1,20,1,20,5,20,417,8,20,10,20,12,20,420,
		9,20,1,20,1,20,1,21,1,21,1,21,1,21,1,21,1,21,1,21,5,21,431,8,21,10,21,
		12,21,434,9,21,1,21,1,21,1,22,1,22,1,22,1,22,1,22,1,22,1,22,1,22,1,22,
		1,22,1,22,1,22,1,22,1,22,1,22,5,22,453,8,22,10,22,12,22,456,9,22,1,22,
		1,22,1,23,1,23,1,23,1,23,1,23,1,23,1,23,1,23,1,23,1,23,1,23,1,23,5,23,
		472,8,23,10,23,12,23,475,9,23,1,23,1,23,1,24,1,24,1,24,1,24,1,24,1,24,
		1,24,1,24,1,24,1,24,1,24,1,24,1,24,5,24,492,8,24,10,24,12,24,495,9,24,
		1,24,1,24,1,25,1,25,1,25,1,25,1,25,1,25,1,25,1,25,1,25,1,25,1,25,1,25,
		1,25,1,25,1,25,1,25,1,25,1,25,1,25,1,25,1,25,1,25,1,25,1,25,5,25,523,8,
		25,10,25,12,25,526,9,25,1,25,1,25,1,26,1,26,1,26,1,26,1,26,1,26,1,26,1,
		26,1,26,1,26,1,26,1,26,1,26,1,26,1,26,1,26,1,26,1,26,1,27,1,27,1,27,1,
		27,1,27,1,27,1,27,1,27,1,27,1,27,1,27,1,27,1,27,1,27,1,27,1,27,1,27,1,
		27,1,27,1,28,1,28,1,28,1,28,1,28,1,28,1,28,1,28,1,28,1,28,1,28,1,28,1,
		28,1,28,1,29,1,29,1,29,1,29,1,29,1,29,1,29,1,29,1,29,1,29,1,29,1,29,1,
		29,1,29,1,29,1,29,1,29,1,30,1,30,1,30,1,30,1,30,1,30,1,30,1,30,1,30,5,
		30,607,8,30,10,30,12,30,610,9,30,1,30,1,30,1,31,1,31,5,31,616,8,31,10,
		31,12,31,619,9,31,1,32,3,32,622,8,32,1,32,1,32,5,32,626,8,32,10,32,12,
		32,629,9,32,1,32,3,32,632,8,32,1,32,1,32,4,32,636,8,32,11,32,12,32,637,
		3,32,640,8,32,1,32,1,32,4,32,644,8,32,11,32,12,32,645,3,32,648,8,32,1,
		32,1,32,3,32,652,8,32,1,32,4,32,655,8,32,11,32,12,32,656,3,32,659,8,32,
		1,32,1,32,5,32,663,8,32,10,32,12,32,666,9,32,1,32,3,32,669,8,32,1,32,3,
		32,672,8,32,1,33,1,33,1,33,5,33,677,8,33,10,33,12,33,680,9,33,1,34,1,34,
		1,34,5,34,685,8,34,10,34,12,34,688,9,34,1,35,1,35,4,35,692,8,35,11,35,
		12,35,693,1,36,1,36,5,36,698,8,36,10,36,12,36,701,9,36,1,36,1,36,1,37,
		1,37,1,38,1,38,1,39,4,39,710,8,39,11,39,12,39,711,1,39,1,39,1,40,3,40,
		717,8,40,1,40,1,40,16,263,297,321,337,354,370,389,403,418,432,454,473,
		493,524,608,699,0,41,1,1,3,2,5,3,7,4,9,5,11,6,13,7,15,8,17,9,19,10,21,
		11,23,12,25,13,27,14,29,15,31,16,33,17,35,18,37,19,39,20,41,21,43,22,45,
		23,47,24,49,25,51,26,53,27,55,28,57,29,59,30,61,31,63,32,65,33,67,34,69,
		35,71,36,73,37,75,38,77,39,79,40,81,0,1,0,7,1,0,49,57,1,0,48,57,2,0,43,
		43,45,45,2,0,69,69,101,101,1,0,65,90,4,0,45,45,48,58,65,90,97,122,3,0,
		9,10,13,13,32,32,759,0,1,1,0,0,0,0,3,1,0,0,0,0,5,1,0,0,0,0,7,1,0,0,0,0,
		9,1,0,0,0,0,11,1,0,0,0,0,13,1,0,0,0,0,15,1,0,0,0,0,17,1,0,0,0,0,19,1,0,
		0,0,0,21,1,0,0,0,0,23,1,0,0,0,0,25,1,0,0,0,0,27,1,0,0,0,0,29,1,0,0,0,0,
		31,1,0,0,0,0,33,1,0,0,0,0,35,1,0,0,0,0,37,1,0,0,0,0,39,1,0,0,0,0,41,1,
		0,0,0,0,43,1,0,0,0,0,45,1,0,0,0,0,47,1,0,0,0,0,49,1,0,0,0,0,51,1,0,0,0,
		0,53,1,0,0,0,0,55,1,0,0,0,0,57,1,0,0,0,0,59,1,0,0,0,0,61,1,0,0,0,0,63,
		1,0,0,0,0,65,1,0,0,0,0,67,1,0,0,0,0,69,1,0,0,0,0,71,1,0,0,0,0,73,1,0,0,
		0,0,75,1,0,0,0,0,77,1,0,0,0,0,79,1,0,0,0,1,83,1,0,0,0,3,92,1,0,0,0,5,94,
		1,0,0,0,7,107,1,0,0,0,9,116,1,0,0,0,11,118,1,0,0,0,13,125,1,0,0,0,15,195,
		1,0,0,0,17,197,1,0,0,0,19,217,1,0,0,0,21,232,1,0,0,0,23,250,1,0,0,0,25,
		268,1,0,0,0,27,282,1,0,0,0,29,302,1,0,0,0,31,326,1,0,0,0,33,342,1,0,0,
		0,35,359,1,0,0,0,37,375,1,0,0,0,39,394,1,0,0,0,41,408,1,0,0,0,43,423,1,
		0,0,0,45,437,1,0,0,0,47,459,1,0,0,0,49,478,1,0,0,0,51,498,1,0,0,0,53,529,
		1,0,0,0,55,547,1,0,0,0,57,566,1,0,0,0,59,580,1,0,0,0,61,597,1,0,0,0,63,
		613,1,0,0,0,65,621,1,0,0,0,67,673,1,0,0,0,69,681,1,0,0,0,71,689,1,0,0,
		0,73,695,1,0,0,0,75,704,1,0,0,0,77,706,1,0,0,0,79,709,1,0,0,0,81,716,1,
		0,0,0,83,84,5,92,0,0,84,85,5,109,0,0,85,86,5,97,0,0,86,87,5,120,0,0,87,
		88,5,105,0,0,88,89,5,109,0,0,89,90,5,117,0,0,90,91,5,109,0,0,91,2,1,0,
		0,0,92,93,5,60,0,0,93,4,1,0,0,0,94,95,5,92,0,0,95,96,5,109,0,0,96,97,5,
		105,0,0,97,98,5,110,0,0,98,99,5,45,0,0,99,100,5,102,0,0,100,101,5,105,
		0,0,101,102,5,101,0,0,102,103,5,108,0,0,103,104,5,100,0,0,104,105,5,115,
		0,0,105,106,5,32,0,0,106,6,1,0,0,0,107,108,5,92,0,0,108,109,5,109,0,0,
		109,110,5,105,0,0,110,111,5,110,0,0,111,112,5,105,0,0,112,113,5,109,0,
		0,113,114,5,117,0,0,114,115,5,109,0,0,115,8,1,0,0,0,116,117,5,62,0,0,117,
		10,1,0,0,0,118,119,5,92,0,0,119,120,5,116,0,0,120,121,5,121,0,0,121,122,
		5,112,0,0,122,123,5,101,0,0,123,124,5,32,0,0,124,12,1,0,0,0,125,126,5,
		92,0,0,126,127,5,117,0,0,127,128,5,110,0,0,128,129,5,105,0,0,129,130,5,
		116,0,0,130,131,5,115,0,0,131,132,5,66,0,0,132,133,5,97,0,0,133,134,5,
		115,0,0,134,135,5,101,0,0,135,136,5,100,0,0,136,137,5,79,0,0,137,138,5,
		110,0,0,138,139,5,70,0,0,139,140,5,105,0,0,140,141,5,101,0,0,141,142,5,
		108,0,0,142,143,5,100,0,0,143,144,5,32,0,0,144,14,1,0,0,0,145,146,5,105,
		0,0,146,147,5,110,0,0,147,148,5,116,0,0,148,149,5,101,0,0,149,150,5,103,
		0,0,150,151,5,101,0,0,151,196,5,114,0,0,152,153,5,114,0,0,153,154,5,101,
		0,0,154,155,5,97,0,0,155,196,5,108,0,0,156,157,5,97,0,0,157,158,5,108,
		0,0,158,159,5,112,0,0,159,160,5,104,0,0,160,196,5,97,0,0,161,162,5,99,
		0,0,162,163,5,104,0,0,163,164,5,111,0,0,164,165,5,105,0,0,165,166,5,99,
		0,0,166,196,5,101,0,0,167,168,5,111,0,0,168,169,5,98,0,0,169,170,5,106,
		0,0,170,171,5,101,0,0,171,172,5,99,0,0,172,173,5,116,0,0,173,174,5,45,
		0,0,174,175,5,108,0,0,175,176,5,105,0,0,176,177,5,115,0,0,177,196,5,116,
		0,0,178,179,5,101,0,0,179,180,5,120,0,0,180,181,5,116,0,0,181,182,5,101,
		0,0,182,183,5,114,0,0,183,184,5,110,0,0,184,185,5,97,0,0,185,186,5,108,
		0,0,186,187,5,45,0,0,187,188,5,108,0,0,188,189,5,105,0,0,189,190,5,115,
		0,0,190,196,5,116,0,0,191,192,5,110,0,0,192,193,5,111,0,0,193,194,5,100,
		0,0,194,196,5,101,0,0,195,145,1,0,0,0,195,152,1,0,0,0,195,156,1,0,0,0,
		195,161,1,0,0,0,195,167,1,0,0,0,195,178,1,0,0,0,195,191,1,0,0,0,196,16,
		1,0,0,0,197,198,5,92,0,0,198,199,5,97,0,0,199,200,5,117,0,0,200,201,5,
		116,0,0,201,202,5,111,0,0,202,203,5,99,0,0,203,204,5,97,0,0,204,205,5,
		108,0,0,205,206,5,99,0,0,206,207,5,117,0,0,207,208,5,108,0,0,208,209,5,
		97,0,0,209,210,5,116,0,0,210,211,5,97,0,0,211,212,5,98,0,0,212,213,5,108,
		0,0,213,214,5,101,0,0,214,215,1,0,0,0,215,216,3,81,40,0,216,18,1,0,0,0,
		217,218,5,92,0,0,218,219,5,97,0,0,219,220,5,117,0,0,220,221,5,116,0,0,
		221,222,5,111,0,0,222,223,5,115,0,0,223,224,5,105,0,0,224,225,5,122,0,
		0,225,226,5,97,0,0,226,227,5,98,0,0,227,228,5,108,0,0,228,229,5,101,0,
		0,229,230,1,0,0,0,230,231,3,81,40,0,231,20,1,0,0,0,232,233,5,92,0,0,233,
		234,5,98,0,0,234,235,5,101,0,0,235,236,5,103,0,0,236,237,5,105,0,0,237,
		238,5,110,0,0,238,239,5,45,0,0,239,240,5,101,0,0,240,241,5,120,0,0,241,
		242,5,116,0,0,242,243,5,101,0,0,243,244,5,110,0,0,244,245,5,115,0,0,245,
		246,5,105,0,0,246,247,5,98,0,0,247,248,5,108,0,0,248,249,5,101,0,0,249,
		22,1,0,0,0,250,251,5,92,0,0,251,252,5,100,0,0,252,253,5,101,0,0,253,254,
		5,102,0,0,254,255,5,97,0,0,255,256,5,117,0,0,256,257,5,108,0,0,257,258,
		5,116,0,0,258,259,5,32,0,0,259,263,1,0,0,0,260,262,9,0,0,0,261,260,1,0,
		0,0,262,265,1,0,0,0,263,264,1,0,0,0,263,261,1,0,0,0,264,266,1,0,0,0,265,
		263,1,0,0,0,266,267,3,81,40,0,267,24,1,0,0,0,268,269,5,92,0,0,269,270,
		5,100,0,0,270,271,5,101,0,0,271,272,5,112,0,0,272,273,5,114,0,0,273,274,
		5,101,0,0,274,275,5,99,0,0,275,276,5,97,0,0,276,277,5,116,0,0,277,278,
		5,101,0,0,278,279,5,100,0,0,279,280,1,0,0,0,280,281,3,81,40,0,281,26,1,
		0,0,0,282,283,5,92,0,0,283,284,5,101,0,0,284,285,5,120,0,0,285,286,5,116,
		0,0,286,287,5,101,0,0,287,288,5,110,0,0,288,289,5,115,0,0,289,290,5,105,
		0,0,290,291,5,98,0,0,291,292,5,108,0,0,292,293,5,101,0,0,293,297,1,0,0,
		0,294,296,9,0,0,0,295,294,1,0,0,0,296,299,1,0,0,0,297,298,1,0,0,0,297,
		295,1,0,0,0,298,300,1,0,0,0,299,297,1,0,0,0,300,301,3,81,40,0,301,28,1,
		0,0,0,302,303,5,92,0,0,303,304,5,101,0,0,304,305,5,120,0,0,305,306,5,116,
		0,0,306,307,5,101,0,0,307,308,5,114,0,0,308,309,5,110,0,0,309,310,5,97,
		0,0,310,311,5,108,0,0,311,312,5,45,0,0,312,313,5,108,0,0,313,314,5,105,
		0,0,314,315,5,115,0,0,315,316,5,116,0,0,316,317,5,32,0,0,317,321,1,0,0,
		0,318,320,9,0,0,0,319,318,1,0,0,0,320,323,1,0,0,0,321,322,1,0,0,0,321,
		319,1,0,0,0,322,324,1,0,0,0,323,321,1,0,0,0,324,325,3,81,40,0,325,30,1,
		0,0,0,326,327,5,92,0,0,327,328,5,102,0,0,328,329,5,105,0,0,329,330,5,101,
		0,0,330,331,5,108,0,0,331,332,5,100,0,0,332,333,5,32,0,0,333,337,1,0,0,
		0,334,336,9,0,0,0,335,334,1,0,0,0,336,339,1,0,0,0,337,338,1,0,0,0,337,
		335,1,0,0,0,338,340,1,0,0,0,339,337,1,0,0,0,340,341,3,81,40,0,341,32,1,
		0,0,0,342,343,5,92,0,0,343,344,5,102,0,0,344,345,5,111,0,0,345,346,5,114,
		0,0,346,347,5,109,0,0,347,348,5,97,0,0,348,349,5,116,0,0,349,350,5,32,
		0,0,350,354,1,0,0,0,351,353,9,0,0,0,352,351,1,0,0,0,353,356,1,0,0,0,354,
		355,1,0,0,0,354,352,1,0,0,0,355,357,1,0,0,0,356,354,1,0,0,0,357,358,3,
		81,40,0,358,34,1,0,0,0,359,360,5,92,0,0,360,361,5,103,0,0,361,362,5,114,
		0,0,362,363,5,111,0,0,363,364,5,117,0,0,364,365,5,112,0,0,365,366,5,32,
		0,0,366,370,1,0,0,0,367,369,9,0,0,0,368,367,1,0,0,0,369,372,1,0,0,0,370,
		371,1,0,0,0,370,368,1,0,0,0,371,373,1,0,0,0,372,370,1,0,0,0,373,374,3,
		81,40,0,374,36,1,0,0,0,375,376,5,92,0,0,376,377,5,105,0,0,377,378,5,112,
		0,0,378,379,5,45,0,0,379,380,5,117,0,0,380,381,5,110,0,0,381,382,5,105,
		0,0,382,383,5,116,0,0,383,384,5,115,0,0,384,385,5,32,0,0,385,389,1,0,0,
		0,386,388,9,0,0,0,387,386,1,0,0,0,388,391,1,0,0,0,389,390,1,0,0,0,389,
		387,1,0,0,0,390,392,1,0,0,0,391,389,1,0,0,0,392,393,3,81,40,0,393,38,1,
		0,0,0,394,395,5,92,0,0,395,396,5,107,0,0,396,397,5,101,0,0,397,398,5,121,
		0,0,398,399,5,32,0,0,399,403,1,0,0,0,400,402,9,0,0,0,401,400,1,0,0,0,402,
		405,1,0,0,0,403,404,1,0,0,0,403,401,1,0,0,0,404,406,1,0,0,0,405,403,1,
		0,0,0,406,407,3,81,40,0,407,40,1,0,0,0,408,409,5,92,0,0,409,410,5,109,
		0,0,410,411,5,101,0,0,411,412,5,109,0,0,412,413,5,111,0,0,413,414,5,32,
		0,0,414,418,1,0,0,0,415,417,9,0,0,0,416,415,1,0,0,0,417,420,1,0,0,0,418,
		419,1,0,0,0,418,416,1,0,0,0,419,421,1,0,0,0,420,418,1,0,0,0,421,422,3,
		81,40,0,422,42,1,0,0,0,423,424,5,92,0,0,424,425,5,110,0,0,425,426,5,111,
		0,0,426,427,5,116,0,0,427,428,5,101,0,0,428,432,1,0,0,0,429,431,9,0,0,
		0,430,429,1,0,0,0,431,434,1,0,0,0,432,433,1,0,0,0,432,430,1,0,0,0,433,
		435,1,0,0,0,434,432,1,0,0,0,435,436,3,81,40,0,436,44,1,0,0,0,437,438,5,
		92,0,0,438,439,5,111,0,0,439,440,5,98,0,0,440,441,5,106,0,0,441,442,5,
		101,0,0,442,443,5,99,0,0,443,444,5,116,0,0,444,445,5,45,0,0,445,446,5,
		108,0,0,446,447,5,105,0,0,447,448,5,115,0,0,448,449,5,116,0,0,449,450,
		5,32,0,0,450,454,1,0,0,0,451,453,9,0,0,0,452,451,1,0,0,0,453,456,1,0,0,
		0,454,455,1,0,0,0,454,452,1,0,0,0,455,457,1,0,0,0,456,454,1,0,0,0,457,
		458,3,81,40,0,458,46,1,0,0,0,459,460,5,92,0,0,460,461,5,111,0,0,461,462,
		5,98,0,0,462,463,5,115,0,0,463,464,5,111,0,0,464,465,5,108,0,0,465,466,
		5,101,0,0,466,467,5,116,0,0,467,468,5,101,0,0,468,469,5,32,0,0,469,473,
		1,0,0,0,470,472,9,0,0,0,471,470,1,0,0,0,472,475,1,0,0,0,473,474,1,0,0,
		0,473,471,1,0,0,0,474,476,1,0,0,0,475,473,1,0,0,0,476,477,3,81,40,0,477,
		48,1,0,0,0,478,479,5,92,0,0,479,480,5,114,0,0,480,481,5,101,0,0,481,482,
		5,102,0,0,482,483,5,101,0,0,483,484,5,114,0,0,484,485,5,101,0,0,485,486,
		5,110,0,0,486,487,5,99,0,0,487,488,5,101,0,0,488,489,5,32,0,0,489,493,
		1,0,0,0,490,492,9,0,0,0,491,490,1,0,0,0,492,495,1,0,0,0,493,494,1,0,0,
		0,493,491,1,0,0,0,494,496,1,0,0,0,495,493,1,0,0,0,496,497,3,81,40,0,497,
		50,1,0,0,0,498,499,5,92,0,0,499,500,5,114,0,0,500,501,5,101,0,0,501,502,
		5,102,0,0,502,503,5,101,0,0,503,504,5,114,0,0,504,505,5,101,0,0,505,506,
		5,110,0,0,506,507,5,99,0,0,507,508,5,101,0,0,508,509,5,45,0,0,509,510,
		5,99,0,0,510,511,5,108,0,0,511,512,5,97,0,0,512,513,5,115,0,0,513,514,
		5,115,0,0,514,515,5,45,0,0,515,516,5,110,0,0,516,517,5,97,0,0,517,518,
		5,109,0,0,518,519,5,101,0,0,519,520,5,32,0,0,520,524,1,0,0,0,521,523,9,
		0,0,0,522,521,1,0,0,0,523,526,1,0,0,0,524,525,1,0,0,0,524,522,1,0,0,0,
		525,527,1,0,0,0,526,524,1,0,0,0,527,528,3,81,40,0,528,52,1,0,0,0,529,530,
		5,92,0,0,530,531,5,114,0,0,531,532,5,101,0,0,532,533,5,113,0,0,533,534,
		5,117,0,0,534,535,5,105,0,0,535,536,5,114,0,0,536,537,5,101,0,0,537,538,
		5,100,0,0,538,539,5,45,0,0,539,540,5,102,0,0,540,541,5,105,0,0,541,542,
		5,101,0,0,542,543,5,108,0,0,543,544,5,100,0,0,544,545,1,0,0,0,545,546,
		3,81,40,0,546,54,1,0,0,0,547,548,5,92,0,0,548,549,5,114,0,0,549,550,5,
		101,0,0,550,551,5,113,0,0,551,552,5,117,0,0,552,553,5,105,0,0,553,554,
		5,114,0,0,554,555,5,101,0,0,555,556,5,100,0,0,556,557,5,45,0,0,557,558,
		5,111,0,0,558,559,5,98,0,0,559,560,5,106,0,0,560,561,5,101,0,0,561,562,
		5,99,0,0,562,563,5,116,0,0,563,564,1,0,0,0,564,565,3,81,40,0,565,56,1,
		0,0,0,566,567,5,92,0,0,567,568,5,114,0,0,568,569,5,101,0,0,569,570,5,116,
		0,0,570,571,5,97,0,0,571,572,5,105,0,0,572,573,5,110,0,0,573,574,5,99,
		0,0,574,575,5,97,0,0,575,576,5,115,0,0,576,577,5,101,0,0,577,578,1,0,0,
		0,578,579,3,81,40,0,579,58,1,0,0,0,580,581,5,92,0,0,581,582,5,117,0,0,
		582,583,5,110,0,0,583,584,5,105,0,0,584,585,5,113,0,0,585,586,5,117,0,
		0,586,587,5,101,0,0,587,588,5,45,0,0,588,589,5,111,0,0,589,590,5,98,0,
		0,590,591,5,106,0,0,591,592,5,101,0,0,592,593,5,99,0,0,593,594,5,116,0,
		0,594,595,1,0,0,0,595,596,3,81,40,0,596,60,1,0,0,0,597,598,5,92,0,0,598,
		599,5,117,0,0,599,600,5,110,0,0,600,601,5,105,0,0,601,602,5,116,0,0,602,
		603,5,115,0,0,603,604,5,32,0,0,604,608,1,0,0,0,605,607,9,0,0,0,606,605,
		1,0,0,0,607,610,1,0,0,0,608,609,1,0,0,0,608,606,1,0,0,0,609,611,1,0,0,
		0,610,608,1,0,0,0,611,612,3,81,40,0,612,62,1,0,0,0,613,617,7,0,0,0,614,
		616,7,1,0,0,615,614,1,0,0,0,616,619,1,0,0,0,617,615,1,0,0,0,617,618,1,
		0,0,0,618,64,1,0,0,0,619,617,1,0,0,0,620,622,7,2,0,0,621,620,1,0,0,0,621,
		622,1,0,0,0,622,671,1,0,0,0,623,627,7,0,0,0,624,626,7,1,0,0,625,624,1,
		0,0,0,626,629,1,0,0,0,627,625,1,0,0,0,627,628,1,0,0,0,628,632,1,0,0,0,
		629,627,1,0,0,0,630,632,5,48,0,0,631,623,1,0,0,0,631,630,1,0,0,0,632,639,
		1,0,0,0,633,635,5,46,0,0,634,636,7,1,0,0,635,634,1,0,0,0,636,637,1,0,0,
		0,637,635,1,0,0,0,637,638,1,0,0,0,638,640,1,0,0,0,639,633,1,0,0,0,639,
		640,1,0,0,0,640,648,1,0,0,0,641,643,5,46,0,0,642,644,7,1,0,0,643,642,1,
		0,0,0,644,645,1,0,0,0,645,643,1,0,0,0,645,646,1,0,0,0,646,648,1,0,0,0,
		647,631,1,0,0,0,647,641,1,0,0,0,648,658,1,0,0,0,649,651,7,3,0,0,650,652,
		5,45,0,0,651,650,1,0,0,0,651,652,1,0,0,0,652,654,1,0,0,0,653,655,7,1,0,
		0,654,653,1,0,0,0,655,656,1,0,0,0,656,654,1,0,0,0,656,657,1,0,0,0,657,
		659,1,0,0,0,658,649,1,0,0,0,658,659,1,0,0,0,659,672,1,0,0,0,660,664,7,
		0,0,0,661,663,7,1,0,0,662,661,1,0,0,0,663,666,1,0,0,0,664,662,1,0,0,0,
		664,665,1,0,0,0,665,669,1,0,0,0,666,664,1,0,0,0,667,669,5,48,0,0,668,660,
		1,0,0,0,668,667,1,0,0,0,669,670,1,0,0,0,670,672,5,46,0,0,671,647,1,0,0,
		0,671,668,1,0,0,0,672,66,1,0,0,0,673,674,5,65,0,0,674,678,7,0,0,0,675,
		677,7,1,0,0,676,675,1,0,0,0,677,680,1,0,0,0,678,676,1,0,0,0,678,679,1,
		0,0,0,679,68,1,0,0,0,680,678,1,0,0,0,681,682,5,78,0,0,682,686,7,0,0,0,
		683,685,7,1,0,0,684,683,1,0,0,0,685,688,1,0,0,0,686,684,1,0,0,0,686,687,
		1,0,0,0,687,70,1,0,0,0,688,686,1,0,0,0,689,691,7,4,0,0,690,692,7,5,0,0,
		691,690,1,0,0,0,692,693,1,0,0,0,693,691,1,0,0,0,693,694,1,0,0,0,694,72,
		1,0,0,0,695,699,5,33,0,0,696,698,9,0,0,0,697,696,1,0,0,0,698,701,1,0,0,
		0,699,700,1,0,0,0,699,697,1,0,0,0,700,702,1,0,0,0,701,699,1,0,0,0,702,
		703,3,81,40,0,703,74,1,0,0,0,704,705,5,44,0,0,705,76,1,0,0,0,706,707,5,
		59,0,0,707,78,1,0,0,0,708,710,7,6,0,0,709,708,1,0,0,0,710,711,1,0,0,0,
		711,709,1,0,0,0,711,712,1,0,0,0,712,713,1,0,0,0,713,714,6,39,0,0,714,80,
		1,0,0,0,715,717,5,13,0,0,716,715,1,0,0,0,716,717,1,0,0,0,717,718,1,0,0,
		0,718,719,5,10,0,0,719,82,1,0,0,0,37,0,195,263,297,321,337,354,370,389,
		403,418,432,454,473,493,524,608,617,621,627,631,637,639,645,647,651,656,
		658,664,668,671,678,686,693,699,711,716,1,6,0,0
	};

	public static readonly ATN _ATN =
		new ATNDeserializer().Deserialize(_serializedATN);


}
