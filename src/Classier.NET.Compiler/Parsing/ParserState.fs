// Copyright (c) 2020 NAME HERE
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

namespace Classier.NET.Compiler

open System.Collections.Immutable

type ParserState =
    { Flags: ImmutableStack<Flags>
      Parents: ImmutableStack<Identifier list>
      Symbols: SymbolTable }

module ParserState =
    let defaultState =
        { Flags = ImmutableStack.Empty
          Parents = ImmutableStack.Empty
          Symbols = SymbolTable.empty }

    let currentFlags state =
        if state.Flags.IsEmpty
        then Flags.None
        else state.Flags.Peek()

    let currentParent state =
        if state.Parents.IsEmpty
        then List.empty
        else state.Parents.Peek()

    let visibilityFlags state = currentFlags state &&& Flags.VisibilityMask

    let pushFlags flags state = { state with Flags = state.Flags.Push(flags) }

    let newFlags state = pushFlags Flags.None state

    let pushParent parent state = { state with Parents = state.Parents.Push(parent) }

    let popFlags state = { state with Flags = state.Flags.Pop() }

    let setFlags flags state =
        let newFlags = currentFlags state ||| flags
        state
        |> popFlags
        |> pushFlags newFlags

    let updateSymbols f state = { state with Symbols = f state.Symbols }

    let clearParents state = { state with Parents = ImmutableStack.Empty }
