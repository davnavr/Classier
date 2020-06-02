namespace Classier.NET.Compiler
{
    using System;
    using System.IO;

    /// <summary>
    /// Provides easy access to an embedded resource.
    /// </summary>
    public sealed class EmbeddedSourceFile
    {
        private readonly string resource;

        public EmbeddedSourceFile(string name) => this.resource = "Classier.NET.Compiler.source." + name;

        public Stream GetStream() =>
            typeof(EmbeddedSourceFile).Assembly.GetManifestResourceStream(this.resource)
            ?? throw new InvalidOperationException($"Unable to find resource {this.resource}");
    }
}
