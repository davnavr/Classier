namespace Classier.NET.Compiler.Grammar
{
    using System;
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

            // Act
            var namespaces = new SuccessResult(compilationUnit, stream, file, Encoding.UTF8).State.Symbols.Namespaces;

            // Assert
            Assert.NotNull(namespaces[ListModule.OfArray(namespaceNames)]);
        }

        [InlineData("MultipleClasses.txt", "test", new[] { "Class1", "Class2", "Interface1", "Class3", "Class4", "Class5", "Class6" })]
        [InlineData("MyGenericClass1.txt", "some.name.collections", new[] { "MutableList<T>" })]
        [Theory]
        public void ParserAddsTypesToSymbolTable(string file, string namespaceName, string[] typeNames)
        {
            // Arrange
            using var stream = new EmbeddedSourceFile(file).GetStream();

            // Act
            var types = new SuccessResult(compilationUnit, stream, file, Encoding.UTF8).State.Symbols
                .Namespaces[ListModule.OfArray(namespaceName.Split('.'))]
                .Select(type => GlobalType.getName(type.Type))
                .ToImmutableSortedSet();

            // Assert
            Assert.True(types.IsSupersetOf(typeNames), $"The type set is missing {string.Join(", ", types.Except(typeNames))}");
        }
    }
}
