// Generated from /home/lucas/ivprog/grammar/pt-br/ivprog.g4 by ANTLR 4.7.1
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class ivprog extends Lexer {
	static { RuntimeMetaData.checkVersion("4.7.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		RK_PROGRAM=1, RK_REAL=2, RK_VOID=3, RK_BOOLEAN=4, RK_STRING=5, RK_INTEGER=6, 
		RK_CHARACTER=7, RK_SWITCH=8, RK_CASE=9, RK_DEFAULT=10, RK_CONST=11, RK_FUNCTION=12, 
		RK_RETURN=13, RK_FOR=14, RK_BREAK=15, RK_DO=16, RK_WHILE=17, RK_IF=18, 
		RK_ELSE=19, RK_FALSE=20, RK_TRUE=21, OPEN_PARENTHESIS=22, CLOSE_PARENTHESIS=23, 
		OPEN_BRACE=24, CLOSE_BRACE=25, OPEN_CURLY=26, CLOSE_CURLY=27, COMMA=28, 
		EQUAL=29, SUM_OP=30, MULTI_OP=31, AND_OPERATOR=32, OR_OPERATOR=33, RELATIONAL_OPERATOR=34, 
		COLON=35, NOT_OPERATOR=36, ID=37, INTEGER=38, REAL=39, STRING=40, CHARACTER=41, 
		WHITESPACE=42, EOS=43, COMMENTS=44;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"RK_PROGRAM", "RK_REAL", "RK_VOID", "RK_BOOLEAN", "RK_STRING", "RK_INTEGER", 
		"RK_CHARACTER", "RK_SWITCH", "RK_CASE", "RK_DEFAULT", "RK_CONST", "RK_FUNCTION", 
		"RK_RETURN", "RK_FOR", "RK_BREAK", "RK_DO", "RK_WHILE", "RK_IF", "RK_ELSE", 
		"RK_FALSE", "RK_TRUE", "RK_LOGICAL_NOT", "RK_LOGICAL_AND", "RK_LOGICAL_OR", 
		"OPEN_PARENTHESIS", "CLOSE_PARENTHESIS", "OPEN_BRACE", "CLOSE_BRACE", 
		"OPEN_CURLY", "CLOSE_CURLY", "COMMA", "EQUAL", "SUM_OP", "MULTI_OP", "AND_OPERATOR", 
		"OR_OPERATOR", "RELATIONAL_OPERATOR", "COLON", "NOT_OPERATOR", "ID", "INTEGER", 
		"REAL", "STRING", "STRING_CHARACTER", "CHARACTER", "WHITESPACE", "SEMICOLON", 
		"EOS", "HEX_DIGIT", "OCTAL_DIGIT", "ESC_SEQ", "ESC_OCTAL", "ESC_UNICODE", 
		"COMMENTS"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'programa'", "'real'", "'vazio'", "'logico'", "'cadeia'", "'inteiro'", 
		"'caractere'", "'escolha'", "'caso'", "'contrario'", "'const'", "'funcao'", 
		"'retorne'", "'para'", "'pare'", "'faca'", "'enquanto'", "'se'", "'senao'", 
		"'falso'", "'verdadeiro'", "'('", "')'", "'['", "']'", "'{'", "'}'", "','", 
		"'='", null, null, null, null, null, "':'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, "RK_PROGRAM", "RK_REAL", "RK_VOID", "RK_BOOLEAN", "RK_STRING", "RK_INTEGER", 
		"RK_CHARACTER", "RK_SWITCH", "RK_CASE", "RK_DEFAULT", "RK_CONST", "RK_FUNCTION", 
		"RK_RETURN", "RK_FOR", "RK_BREAK", "RK_DO", "RK_WHILE", "RK_IF", "RK_ELSE", 
		"RK_FALSE", "RK_TRUE", "OPEN_PARENTHESIS", "CLOSE_PARENTHESIS", "OPEN_BRACE", 
		"CLOSE_BRACE", "OPEN_CURLY", "CLOSE_CURLY", "COMMA", "EQUAL", "SUM_OP", 
		"MULTI_OP", "AND_OPERATOR", "OR_OPERATOR", "RELATIONAL_OPERATOR", "COLON", 
		"NOT_OPERATOR", "ID", "INTEGER", "REAL", "STRING", "CHARACTER", "WHITESPACE", 
		"EOS", "COMMENTS"
	};
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}


	  //Translate to fit your language
	  ivprog.MAIN_FUNCTION_NAME = "inicio";


	public ivprog(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "ivprog.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2.\u01ba\b\1\4\2\t"+
		"\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \4!"+
		"\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\4(\t(\4)\t)\4*\t*\4+\t+\4"+
		",\t,\4-\t-\4.\t.\4/\t/\4\60\t\60\4\61\t\61\4\62\t\62\4\63\t\63\4\64\t"+
		"\64\4\65\t\65\4\66\t\66\4\67\t\67\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2"+
		"\3\3\3\3\3\3\3\3\3\3\3\4\3\4\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5\3\5\3\5\3"+
		"\5\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\b\3\b"+
		"\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\n\3"+
		"\n\3\n\3\n\3\n\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\f\3"+
		"\f\3\f\3\f\3\f\3\f\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\16\3\16\3\16\3\16\3\16"+
		"\3\16\3\16\3\16\3\17\3\17\3\17\3\17\3\17\3\20\3\20\3\20\3\20\3\20\3\21"+
		"\3\21\3\21\3\21\3\21\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\23"+
		"\3\23\3\23\3\24\3\24\3\24\3\24\3\24\3\24\3\25\3\25\3\25\3\25\3\25\3\25"+
		"\3\26\3\26\3\26\3\26\3\26\3\26\3\26\3\26\3\26\3\26\3\26\3\27\3\27\3\27"+
		"\3\27\3\30\3\30\3\31\3\31\3\31\3\32\3\32\3\33\3\33\3\34\3\34\3\35\3\35"+
		"\3\36\3\36\3\37\3\37\3 \3 \3!\3!\3\"\3\"\3#\3#\3$\3$\3%\3%\3&\3&\3&\3"+
		"&\3&\3&\3&\5&\u012a\n&\3\'\3\'\3(\3(\3)\3)\7)\u0132\n)\f)\16)\u0135\13"+
		")\3*\6*\u0138\n*\r*\16*\u0139\3*\3*\3*\3*\5*\u0140\n*\3*\6*\u0143\n*\r"+
		"*\16*\u0144\3*\3*\3*\3*\5*\u014b\n*\3*\6*\u014e\n*\r*\16*\u014f\5*\u0152"+
		"\n*\3+\6+\u0155\n+\r+\16+\u0156\3+\3+\6+\u015b\n+\r+\16+\u015c\3,\3,\7"+
		",\u0161\n,\f,\16,\u0164\13,\3,\3,\3-\3-\5-\u016a\n-\3.\3.\3.\5.\u016f"+
		"\n.\3.\3.\3/\3/\3/\3/\3\60\3\60\3\61\6\61\u017a\n\61\r\61\16\61\u017b"+
		"\3\61\5\61\u017f\n\61\3\62\3\62\3\63\3\63\3\64\3\64\3\64\3\64\5\64\u0189"+
		"\n\64\3\65\3\65\3\65\3\65\3\65\3\65\3\65\3\65\3\65\3\65\3\65\5\65\u0196"+
		"\n\65\3\66\3\66\3\66\3\66\3\66\3\66\3\66\3\67\3\67\3\67\3\67\7\67\u01a3"+
		"\n\67\f\67\16\67\u01a6\13\67\3\67\5\67\u01a9\n\67\3\67\3\67\3\67\3\67"+
		"\3\67\7\67\u01b0\n\67\f\67\16\67\u01b3\13\67\3\67\3\67\5\67\u01b7\n\67"+
		"\3\67\3\67\3\u01b1\28\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21\n\23\13\25\f\27"+
		"\r\31\16\33\17\35\20\37\21!\22#\23%\24\'\25)\26+\27-\2/\2\61\2\63\30\65"+
		"\31\67\329\33;\34=\35?\36A\37C E!G\"I#K$M%O&Q\'S(U)W*Y\2[+],_\2a-c\2e"+
		"\2g\2i\2k\2m.\3\2\21\4\2--//\5\2\'\',,\61\61\4\2>>@@\5\2C\\aac|\6\2\62"+
		";C\\aac|\3\2\62;\3\2\62\63\6\2\f\f\17\17$$^^\6\2\f\f\17\17))^^\4\2\13"+
		"\13\"\"\4\2\f\f\17\17\5\2\62;CHch\3\2\629\n\2$$))^^ddhhppttvv\3\2\62\65"+
		"\2\u01c9\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2"+
		"\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27"+
		"\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2"+
		"\2\2#\3\2\2\2\2%\3\2\2\2\2\'\3\2\2\2\2)\3\2\2\2\2+\3\2\2\2\2\63\3\2\2"+
		"\2\2\65\3\2\2\2\2\67\3\2\2\2\29\3\2\2\2\2;\3\2\2\2\2=\3\2\2\2\2?\3\2\2"+
		"\2\2A\3\2\2\2\2C\3\2\2\2\2E\3\2\2\2\2G\3\2\2\2\2I\3\2\2\2\2K\3\2\2\2\2"+
		"M\3\2\2\2\2O\3\2\2\2\2Q\3\2\2\2\2S\3\2\2\2\2U\3\2\2\2\2W\3\2\2\2\2[\3"+
		"\2\2\2\2]\3\2\2\2\2a\3\2\2\2\2m\3\2\2\2\3o\3\2\2\2\5x\3\2\2\2\7}\3\2\2"+
		"\2\t\u0083\3\2\2\2\13\u008a\3\2\2\2\r\u0091\3\2\2\2\17\u0099\3\2\2\2\21"+
		"\u00a3\3\2\2\2\23\u00ab\3\2\2\2\25\u00b0\3\2\2\2\27\u00ba\3\2\2\2\31\u00c0"+
		"\3\2\2\2\33\u00c7\3\2\2\2\35\u00cf\3\2\2\2\37\u00d4\3\2\2\2!\u00d9\3\2"+
		"\2\2#\u00de\3\2\2\2%\u00e7\3\2\2\2\'\u00ea\3\2\2\2)\u00f0\3\2\2\2+\u00f6"+
		"\3\2\2\2-\u0101\3\2\2\2/\u0105\3\2\2\2\61\u0107\3\2\2\2\63\u010a\3\2\2"+
		"\2\65\u010c\3\2\2\2\67\u010e\3\2\2\29\u0110\3\2\2\2;\u0112\3\2\2\2=\u0114"+
		"\3\2\2\2?\u0116\3\2\2\2A\u0118\3\2\2\2C\u011a\3\2\2\2E\u011c\3\2\2\2G"+
		"\u011e\3\2\2\2I\u0120\3\2\2\2K\u0129\3\2\2\2M\u012b\3\2\2\2O\u012d\3\2"+
		"\2\2Q\u012f\3\2\2\2S\u0151\3\2\2\2U\u0154\3\2\2\2W\u015e\3\2\2\2Y\u0169"+
		"\3\2\2\2[\u016b\3\2\2\2]\u0172\3\2\2\2_\u0176\3\2\2\2a\u017e\3\2\2\2c"+
		"\u0180\3\2\2\2e\u0182\3\2\2\2g\u0188\3\2\2\2i\u0195\3\2\2\2k\u0197\3\2"+
		"\2\2m\u01b6\3\2\2\2op\7r\2\2pq\7t\2\2qr\7q\2\2rs\7i\2\2st\7t\2\2tu\7c"+
		"\2\2uv\7o\2\2vw\7c\2\2w\4\3\2\2\2xy\7t\2\2yz\7g\2\2z{\7c\2\2{|\7n\2\2"+
		"|\6\3\2\2\2}~\7x\2\2~\177\7c\2\2\177\u0080\7|\2\2\u0080\u0081\7k\2\2\u0081"+
		"\u0082\7q\2\2\u0082\b\3\2\2\2\u0083\u0084\7n\2\2\u0084\u0085\7q\2\2\u0085"+
		"\u0086\7i\2\2\u0086\u0087\7k\2\2\u0087\u0088\7e\2\2\u0088\u0089\7q\2\2"+
		"\u0089\n\3\2\2\2\u008a\u008b\7e\2\2\u008b\u008c\7c\2\2\u008c\u008d\7f"+
		"\2\2\u008d\u008e\7g\2\2\u008e\u008f\7k\2\2\u008f\u0090\7c\2\2\u0090\f"+
		"\3\2\2\2\u0091\u0092\7k\2\2\u0092\u0093\7p\2\2\u0093\u0094\7v\2\2\u0094"+
		"\u0095\7g\2\2\u0095\u0096\7k\2\2\u0096\u0097\7t\2\2\u0097\u0098\7q\2\2"+
		"\u0098\16\3\2\2\2\u0099\u009a\7e\2\2\u009a\u009b\7c\2\2\u009b\u009c\7"+
		"t\2\2\u009c\u009d\7c\2\2\u009d\u009e\7e\2\2\u009e\u009f\7v\2\2\u009f\u00a0"+
		"\7g\2\2\u00a0\u00a1\7t\2\2\u00a1\u00a2\7g\2\2\u00a2\20\3\2\2\2\u00a3\u00a4"+
		"\7g\2\2\u00a4\u00a5\7u\2\2\u00a5\u00a6\7e\2\2\u00a6\u00a7\7q\2\2\u00a7"+
		"\u00a8\7n\2\2\u00a8\u00a9\7j\2\2\u00a9\u00aa\7c\2\2\u00aa\22\3\2\2\2\u00ab"+
		"\u00ac\7e\2\2\u00ac\u00ad\7c\2\2\u00ad\u00ae\7u\2\2\u00ae\u00af\7q\2\2"+
		"\u00af\24\3\2\2\2\u00b0\u00b1\7e\2\2\u00b1\u00b2\7q\2\2\u00b2\u00b3\7"+
		"p\2\2\u00b3\u00b4\7v\2\2\u00b4\u00b5\7t\2\2\u00b5\u00b6\7c\2\2\u00b6\u00b7"+
		"\7t\2\2\u00b7\u00b8\7k\2\2\u00b8\u00b9\7q\2\2\u00b9\26\3\2\2\2\u00ba\u00bb"+
		"\7e\2\2\u00bb\u00bc\7q\2\2\u00bc\u00bd\7p\2\2\u00bd\u00be\7u\2\2\u00be"+
		"\u00bf\7v\2\2\u00bf\30\3\2\2\2\u00c0\u00c1\7h\2\2\u00c1\u00c2\7w\2\2\u00c2"+
		"\u00c3\7p\2\2\u00c3\u00c4\7e\2\2\u00c4\u00c5\7c\2\2\u00c5\u00c6\7q\2\2"+
		"\u00c6\32\3\2\2\2\u00c7\u00c8\7t\2\2\u00c8\u00c9\7g\2\2\u00c9\u00ca\7"+
		"v\2\2\u00ca\u00cb\7q\2\2\u00cb\u00cc\7t\2\2\u00cc\u00cd\7p\2\2\u00cd\u00ce"+
		"\7g\2\2\u00ce\34\3\2\2\2\u00cf\u00d0\7r\2\2\u00d0\u00d1\7c\2\2\u00d1\u00d2"+
		"\7t\2\2\u00d2\u00d3\7c\2\2\u00d3\36\3\2\2\2\u00d4\u00d5\7r\2\2\u00d5\u00d6"+
		"\7c\2\2\u00d6\u00d7\7t\2\2\u00d7\u00d8\7g\2\2\u00d8 \3\2\2\2\u00d9\u00da"+
		"\7h\2\2\u00da\u00db\7c\2\2\u00db\u00dc\7e\2\2\u00dc\u00dd\7c\2\2\u00dd"+
		"\"\3\2\2\2\u00de\u00df\7g\2\2\u00df\u00e0\7p\2\2\u00e0\u00e1\7s\2\2\u00e1"+
		"\u00e2\7w\2\2\u00e2\u00e3\7c\2\2\u00e3\u00e4\7p\2\2\u00e4\u00e5\7v\2\2"+
		"\u00e5\u00e6\7q\2\2\u00e6$\3\2\2\2\u00e7\u00e8\7u\2\2\u00e8\u00e9\7g\2"+
		"\2\u00e9&\3\2\2\2\u00ea\u00eb\7u\2\2\u00eb\u00ec\7g\2\2\u00ec\u00ed\7"+
		"p\2\2\u00ed\u00ee\7c\2\2\u00ee\u00ef\7q\2\2\u00ef(\3\2\2\2\u00f0\u00f1"+
		"\7h\2\2\u00f1\u00f2\7c\2\2\u00f2\u00f3\7n\2\2\u00f3\u00f4\7u\2\2\u00f4"+
		"\u00f5\7q\2\2\u00f5*\3\2\2\2\u00f6\u00f7\7x\2\2\u00f7\u00f8\7g\2\2\u00f8"+
		"\u00f9\7t\2\2\u00f9\u00fa\7f\2\2\u00fa\u00fb\7c\2\2\u00fb\u00fc\7f\2\2"+
		"\u00fc\u00fd\7g\2\2\u00fd\u00fe\7k\2\2\u00fe\u00ff\7t\2\2\u00ff\u0100"+
		"\7q\2\2\u0100,\3\2\2\2\u0101\u0102\7p\2\2\u0102\u0103\7c\2\2\u0103\u0104"+
		"\7q\2\2\u0104.\3\2\2\2\u0105\u0106\7G\2\2\u0106\60\3\2\2\2\u0107\u0108"+
		"\7Q\2\2\u0108\u0109\7W\2\2\u0109\62\3\2\2\2\u010a\u010b\7*\2\2\u010b\64"+
		"\3\2\2\2\u010c\u010d\7+\2\2\u010d\66\3\2\2\2\u010e\u010f\7]\2\2\u010f"+
		"8\3\2\2\2\u0110\u0111\7_\2\2\u0111:\3\2\2\2\u0112\u0113\7}\2\2\u0113<"+
		"\3\2\2\2\u0114\u0115\7\177\2\2\u0115>\3\2\2\2\u0116\u0117\7.\2\2\u0117"+
		"@\3\2\2\2\u0118\u0119\7?\2\2\u0119B\3\2\2\2\u011a\u011b\t\2\2\2\u011b"+
		"D\3\2\2\2\u011c\u011d\t\3\2\2\u011dF\3\2\2\2\u011e\u011f\5/\30\2\u011f"+
		"H\3\2\2\2\u0120\u0121\5\61\31\2\u0121J\3\2\2\2\u0122\u0123\7@\2\2\u0123"+
		"\u012a\7?\2\2\u0124\u0125\7?\2\2\u0125\u012a\7?\2\2\u0126\u0127\7>\2\2"+
		"\u0127\u012a\7?\2\2\u0128\u012a\t\4\2\2\u0129\u0122\3\2\2\2\u0129\u0124"+
		"\3\2\2\2\u0129\u0126\3\2\2\2\u0129\u0128\3\2\2\2\u012aL\3\2\2\2\u012b"+
		"\u012c\7<\2\2\u012cN\3\2\2\2\u012d\u012e\5-\27\2\u012eP\3\2\2\2\u012f"+
		"\u0133\t\5\2\2\u0130\u0132\t\6\2\2\u0131\u0130\3\2\2\2\u0132\u0135\3\2"+
		"\2\2\u0133\u0131\3\2\2\2\u0133\u0134\3\2\2\2\u0134R\3\2\2\2\u0135\u0133"+
		"\3\2\2\2\u0136\u0138\t\7\2\2\u0137\u0136\3\2\2\2\u0138\u0139\3\2\2\2\u0139"+
		"\u0137\3\2\2\2\u0139\u013a\3\2\2\2\u013a\u0152\3\2\2\2\u013b\u013c\7\62"+
		"\2\2\u013c\u0140\7z\2\2\u013d\u013e\7\62\2\2\u013e\u0140\7Z\2\2\u013f"+
		"\u013b\3\2\2\2\u013f\u013d\3\2\2\2\u0140\u0142\3\2\2\2\u0141\u0143\5c"+
		"\62\2\u0142\u0141\3\2\2\2\u0143\u0144\3\2\2\2\u0144\u0142\3\2\2\2\u0144"+
		"\u0145\3\2\2\2\u0145\u0152\3\2\2\2\u0146\u0147\7\62\2\2\u0147\u014b\7"+
		"d\2\2\u0148\u0149\7\62\2\2\u0149\u014b\7D\2\2\u014a\u0146\3\2\2\2\u014a"+
		"\u0148\3\2\2\2\u014b\u014d\3\2\2\2\u014c\u014e\t\b\2\2\u014d\u014c\3\2"+
		"\2\2\u014e\u014f\3\2\2\2\u014f\u014d\3\2\2\2\u014f\u0150\3\2\2\2\u0150"+
		"\u0152\3\2\2\2\u0151\u0137\3\2\2\2\u0151\u013f\3\2\2\2\u0151\u014a\3\2"+
		"\2\2\u0152T\3\2\2\2\u0153\u0155\t\7\2\2\u0154\u0153\3\2\2\2\u0155\u0156"+
		"\3\2\2\2\u0156\u0154\3\2\2\2\u0156\u0157\3\2\2\2\u0157\u0158\3\2\2\2\u0158"+
		"\u015a\7\60\2\2\u0159\u015b\t\7\2\2\u015a\u0159\3\2\2\2\u015b\u015c\3"+
		"\2\2\2\u015c\u015a\3\2\2\2\u015c\u015d\3\2\2\2\u015dV\3\2\2\2\u015e\u0162"+
		"\7$\2\2\u015f\u0161\5Y-\2\u0160\u015f\3\2\2\2\u0161\u0164\3\2\2\2\u0162"+
		"\u0160\3\2\2\2\u0162\u0163\3\2\2\2\u0163\u0165\3\2\2\2\u0164\u0162\3\2"+
		"\2\2\u0165\u0166\7$\2\2\u0166X\3\2\2\2\u0167\u016a\n\t\2\2\u0168\u016a"+
		"\5g\64\2\u0169\u0167\3\2\2\2\u0169\u0168\3\2\2\2\u016aZ\3\2\2\2\u016b"+
		"\u016e\7)\2\2\u016c\u016f\5g\64\2\u016d\u016f\n\n\2\2\u016e\u016c\3\2"+
		"\2\2\u016e\u016d\3\2\2\2\u016f\u0170\3\2\2\2\u0170\u0171\7)\2\2\u0171"+
		"\\\3\2\2\2\u0172\u0173\t\13\2\2\u0173\u0174\3\2\2\2\u0174\u0175\b/\2\2"+
		"\u0175^\3\2\2\2\u0176\u0177\7=\2\2\u0177`\3\2\2\2\u0178\u017a\t\f\2\2"+
		"\u0179\u0178\3\2\2\2\u017a\u017b\3\2\2\2\u017b\u0179\3\2\2\2\u017b\u017c"+
		"\3\2\2\2\u017c\u017f\3\2\2\2\u017d\u017f\5_\60\2\u017e\u0179\3\2\2\2\u017e"+
		"\u017d\3\2\2\2\u017fb\3\2\2\2\u0180\u0181\t\r\2\2\u0181d\3\2\2\2\u0182"+
		"\u0183\t\16\2\2\u0183f\3\2\2\2\u0184\u0185\7^\2\2\u0185\u0189\t\17\2\2"+
		"\u0186\u0189\5k\66\2\u0187\u0189\5i\65\2\u0188\u0184\3\2\2\2\u0188\u0186"+
		"\3\2\2\2\u0188\u0187\3\2\2\2\u0189h\3\2\2\2\u018a\u018b\7^\2\2\u018b\u018c"+
		"\t\20\2\2\u018c\u018d\5e\63\2\u018d\u018e\5e\63\2\u018e\u0196\3\2\2\2"+
		"\u018f\u0190\7^\2\2\u0190\u0191\5e\63\2\u0191\u0192\5e\63\2\u0192\u0196"+
		"\3\2\2\2\u0193\u0194\7^\2\2\u0194\u0196\5e\63\2\u0195\u018a\3\2\2\2\u0195"+
		"\u018f\3\2\2\2\u0195\u0193\3\2\2\2\u0196j\3\2\2\2\u0197\u0198\7^\2\2\u0198"+
		"\u0199\7w\2\2\u0199\u019a\5c\62\2\u019a\u019b\5c\62\2\u019b\u019c\5c\62"+
		"\2\u019c\u019d\5c\62\2\u019dl\3\2\2\2\u019e\u019f\7\61\2\2\u019f\u01a0"+
		"\7\61\2\2\u01a0\u01a4\3\2\2\2\u01a1\u01a3\n\f\2\2\u01a2\u01a1\3\2\2\2"+
		"\u01a3\u01a6\3\2\2\2\u01a4\u01a2\3\2\2\2\u01a4\u01a5\3\2\2\2\u01a5\u01a8"+
		"\3\2\2\2\u01a6\u01a4\3\2\2\2\u01a7\u01a9\7\17\2\2\u01a8\u01a7\3\2\2\2"+
		"\u01a8\u01a9\3\2\2\2\u01a9\u01aa\3\2\2\2\u01aa\u01b7\7\f\2\2\u01ab\u01ac"+
		"\7\61\2\2\u01ac\u01ad\7,\2\2\u01ad\u01b1\3\2\2\2\u01ae\u01b0\13\2\2\2"+
		"\u01af\u01ae\3\2\2\2\u01b0\u01b3\3\2\2\2\u01b1\u01b2\3\2\2\2\u01b1\u01af"+
		"\3\2\2\2\u01b2\u01b4\3\2\2\2\u01b3\u01b1\3\2\2\2\u01b4\u01b5\7,\2\2\u01b5"+
		"\u01b7\7\61\2\2\u01b6\u019e\3\2\2\2\u01b6\u01ab\3\2\2\2\u01b7\u01b8\3"+
		"\2\2\2\u01b8\u01b9\b\67\3\2\u01b9n\3\2\2\2\30\2\u0129\u0133\u0139\u013f"+
		"\u0144\u014a\u014f\u0151\u0156\u015c\u0162\u0169\u016e\u017b\u017e\u0188"+
		"\u0195\u01a4\u01a8\u01b1\u01b6\4\b\2\2\2\3\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}