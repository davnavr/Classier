namespace Classier.NET.Compiler
{
    using System;
    using System.IO;
    using System.Text;
    using FParsec;
    using Microsoft.FSharp.Core;
    using static Classier.NET.Compiler.Parsing.Grammar;
    using static FParsec.CharParsers;

    /// <summary>
    /// Wraps a parser result and assumes it is a success.
    /// </summary>
    public sealed class SuccessResult
    {
        private readonly ParserResult<CompilationUnit, ParserState> result;

        public SuccessResult(Func<ParserResult<CompilationUnit, ParserState>> resultEvaluator)
        {
            this.result = resultEvaluator();
        }

        public SuccessResult(FSharpFunc<CharStream<ParserState>, Reply<CompilationUnit>> parser, Stream stream, string streamName, Encoding encoding)
            : this(() => runParserOnStream(parser, ParserStateModule.defaultState, streamName, stream, encoding))
        {
        }

        public CompilationUnit Result => this.CastResult().Item1;

        public ParserState State => this.CastResult().Item2;

        private ParserResult<CompilationUnit, ParserState>.Success CastResult()
        {
            return this.result switch
            {
                ParserResult<CompilationUnit, ParserState>.Success success => success,
                ParserResult<CompilationUnit, ParserState>.Failure failure => throw new InvalidCastException(failure.Item2.ToString()),
                _ => throw new InvalidOperationException($"Unknown result type {this.result.GetType()}."),
            };
        }
    }
}
