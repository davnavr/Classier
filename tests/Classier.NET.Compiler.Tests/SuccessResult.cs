namespace Classier.NET.Compiler
{
    using System;
    using System.IO;
    using System.Text;
    using FParsec;
    using Microsoft.FSharp.Core;
    using static Classier.NET.Compiler.Grammar;
    using static Classier.NET.Compiler.ParserState;
    using static FParsec.CharParsers;

    /// <summary>
    /// Wraps a parser result and assumes it is a success.
    /// </summary>
    public sealed class SuccessResult
    {
        private readonly ParserResult<CompilationUnit, ParserState<Validator>> result;

        public SuccessResult(Func<ParserResult<CompilationUnit, ParserState<Validator>>> resultEvaluator)
        {
            this.result = resultEvaluator();
        }

        public SuccessResult(FSharpFunc<CharStream<ParserState<Validator>>, Reply<CompilationUnit>> parser, Stream stream, string streamName, Encoding encoding)
            : this(() => runParserOnStream(parser, defaultState<Validator>(), streamName, stream, encoding))
        {
        }

        public CompilationUnit Result => this.CastResult().Item1;

        public ParserState<Validator> State => this.CastResult().Item2;

        private ParserResult<CompilationUnit, ParserState<Validator>>.Success CastResult()
        {
            return this.result switch
            {
                ParserResult<CompilationUnit, ParserState<Validator>>.Success success => success,
                ParserResult<CompilationUnit, ParserState<Validator>>.Failure failure => throw new InvalidCastException(failure.Item2.ToString()),
                _ => throw new InvalidOperationException($"Unknown result type {this.result.GetType()}."),
            };
        }
    }
}
