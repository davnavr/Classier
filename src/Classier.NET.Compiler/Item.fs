// Copyright (c) 2020 David Navarro
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

module Classier.NET.Compiler.Item

/// Represents an item in a sequence.
type Item<'T> =
    { Value: 'T
      Index: int
      Next: Lazy<Item<'T> option> }

/// Returns an item containing the first element of the specified sequence.
let fromSeq (source: seq<'T>): Item<'T> option =
    let enumerator = source.GetEnumerator()
    let rec next index =
        if enumerator.MoveNext() then
            Some { Value = enumerator.Current;
                   Index = index;
                   Next = lazy next (index + 1) }
        else
            enumerator.Dispose() |> ignore
            None
    next 0

/// Returns a sequence ranging from the specified start item to the last item.
let toSeq (start: Item<'T> option) =
    let next (item: Item<'T> option) =
        match item with
        | Some currentItem ->
            Some ((currentItem.Value, currentItem.Index), currentItem.Next.Value)
        | None -> None
    Seq.unfold next start
