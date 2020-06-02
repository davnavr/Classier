namespace Classier.NET.Compiler.Grammar
{
    using System.Collections.Generic;
    using System.Collections.Immutable;
    using System.Linq;
    using System.Text;
    using Microsoft.FSharp.Collections;
    using Xunit;
    using static Classier.NET.Compiler.Grammar.Parser;
    using static FParsec.CharParsers;

    public class ParserTests
    {
        [InlineData("MultipleClasses.txt")]
        [InlineData("MyAbstractClass1.txt", "java.lang", "java.util")]
        [InlineData("MyException1.txt")]
        [InlineData("MyModule1.txt", "system.reflection.Assembly")]
        [Theory]
        public void ParserCorrectlySetsUsings(string name, params string[] usings)
        {
            // Arrange
            using var stream = new EmbeddedSourceFile(name).GetStream();

            // Act
            var result = new SuccessResult(compilationUnit, stream, name, Encoding.UTF8).Result;

            // Assert
            Assert.Equal(usings, result.Usings.Select(names => string.Join('.', names)));
        }

        [InlineData("MyClass1.txt", new string[0])]
        [InlineData("MyGenericClass1.txt", new[] { "some", "name", "collections" })]
        [InlineData("NoAccessModifiers.txt", new[] { "My", "Awesome", "Project" })]
        [Theory]
        public void ParserCorrectlySetsNamespace(string name, string[] namespaceName)
        {
            // Arrange
            using var stream = new EmbeddedSourceFile(name).GetStream();

            // Act
            var result = new SuccessResult(compilationUnit, stream, name, Encoding.UTF8).Result;

            // Assert
            Assert.Equal(namespaceName, result.Namespace);
        }

        [InlineData("MissingBrackets1.txt", "closing bracket")]
        [InlineData("NoCatchOrFinally.txt", "at least one catch")]
        [InlineData("AbstractSealedMethod.txt", "not allowed on abstract methods")]
        [Theory]
        public void ParserHasPredictedError(string name, string errorSubstring)
        {
            // Arrange
            using var stream = new EmbeddedSourceFile(name).GetStream();

            // Act
            var error = (ParserResult<CompilationUnit, ParserState>.Failure)runParserOnStream(compilationUnit, ParserStateModule.defaultState, name, stream, Encoding.UTF8);

            // Assert
            Assert.Contains(errorSubstring, error.Item1);
        }

        [InlineData("MyException1.txt", new string[0])]
        [InlineData("MultipleClasses.txt", new[] { "test" })]
        [InlineData("MyAbstractClass1.txt", new[] { "this", "is", "my", "space" })]
        [Theory]
        public void ParserIncludesNamespacesInSymbolTable(string file, string[] namespaceNames)
        {
            // Arrange
            using var stream = new EmbeddedSourceFile(file).GetStream();
            var expectedNamespaces = namespaceNames
                .Aggregate(
                    ImmutableList.Create<FSharpList<string>>(),
                    (list, name) =>
                    {
                        return list.Add(
                            ListModule.Append(
                                list.IsEmpty ? ListModule.Empty<string>() : list[list.Count - 1],
                                ListModule.Singleton(name)));
                    });

            // Act
            var namespaces = new SuccessResult(compilationUnit, stream, file, Encoding.UTF8).State.Symbols.Namespaces;

            // Assert
            Assert.Equal(namespaces.Keys, expectedNamespaces);
        }
    }
}
