//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     ANTLR Version: 4.8
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

// Generated from Idf.g4 by ANTLR 4.8

// Unreachable code detected
#pragma warning disable 0162
// The variable '...' is assigned but its value is never used
#pragma warning disable 0219
// Missing XML comment for publicly visible type or member '...'
#pragma warning disable 1591
// Ambiguous reference in cref attribute
#pragma warning disable 419

using Antlr4.Runtime.Misc;
using IParseTreeListener = Antlr4.Runtime.Tree.IParseTreeListener;
using IToken = Antlr4.Runtime.IToken;

/// <summary>
/// This interface defines a complete listener for a parse tree produced by
/// <see cref="IdfParser"/>.
/// </summary>
[System.CodeDom.Compiler.GeneratedCode("ANTLR", "4.8")]
[System.CLSCompliant(false)]
public interface IIdfListener : IParseTreeListener {
	/// <summary>
	/// Enter a parse tree produced by <see cref="IdfParser.idf"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterIdf([NotNull] IdfParser.IdfContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="IdfParser.idf"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitIdf([NotNull] IdfParser.IdfContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="IdfParser.object"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterObject([NotNull] IdfParser.ObjectContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="IdfParser.object"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitObject([NotNull] IdfParser.ObjectContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="IdfParser.fields"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterFields([NotNull] IdfParser.FieldsContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="IdfParser.fields"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitFields([NotNull] IdfParser.FieldsContext context);
	/// <summary>
	/// Enter a parse tree produced by <see cref="IdfParser.field"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void EnterField([NotNull] IdfParser.FieldContext context);
	/// <summary>
	/// Exit a parse tree produced by <see cref="IdfParser.field"/>.
	/// </summary>
	/// <param name="context">The parse tree.</param>
	void ExitField([NotNull] IdfParser.FieldContext context);
}
