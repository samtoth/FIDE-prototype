//
//  treeable : a -> treeable
//      field
//        children : [Treeable]
//        folded : Bool 
//        tag : Tag a
//
//
//  interface Tag where
//       name : String
//
import produce, {immerable} from "immer"
import {Either} from "fp-ts/Either"
import {TreeUUID} from "features/formDefinition";
import {Context, FIDETree, FormCore} from "./FIDE-lang";

export class Treeable<T extends BasicTag, Children> {
    [immerable] = true

    tag: T;
    folded: boolean;
    children: Children;

    constructor(tag: T, folded: boolean, children: Children) {
        this.tag = tag
        this.folded = folded
        this.children = children
    }
}

export type TreeIndex = number[]

export const idx_eql = (id1: TreeIndex, id2: TreeIndex): boolean => {
    return index_to_string(id1) === index_to_string(id2)
}

export const index_to_string: (c: TreeIndex) => string = (coord: TreeIndex) => {
    if (coord.length && coord.length > 0) {
        const coordCopy = coord
        return coordCopy.map((val, _) => val.toString()).join('.')
    } else {
        return ""
    }
}

export interface BasicTag {
    name: string
}

export interface DisplayProps {
    active: boolean,
    highlights: TreeIndex[],
    treeId: TreeUUID,
    context: Context
}


export interface Displayable<T extends BasicTag, C> extends Treeable<T, C> {
    display: (coord: TreeIndex, props: DisplayProps) => JSX.Element
}

export interface Desugarable<T extends BasicTag, C> extends Treeable<T, C> {
    desugar: () => FormCore
}


export type TypeCheckError = string
export interface Typable<T extends BasicTag, C> extends Treeable<T, C> {
    typeCheck<T extends BasicTag, C>(expectedType: Treeable<T, C>, context: Context): Either<TypeCheckError, []>

    typeInfer(context: Ctx): Either<TypeCheckError, Treeable<BasicTag, any>>
}


// Coord = [int]
// editAt : Coord -> (Tree -> Tree) -> Tree -> Tree
export const editAt:
    <T extends BasicTag, C>(coords: TreeIndex[],
        tf: <T1 extends BasicTag, C1>(t: Treeable<T, C>
        ) => Treeable<T1, C1>, tree: Treeable<T, C>) => Treeable<T1, C1>
    = (coords, treeFunc, tree) => {
        if (coords.length === 0) {
            return tree
        } else {
            const [coord, ...rest] = coords
            let f = ((fix) => (lcoord: TreeIndex, ltreeFunc, ltree) => {
                if (lcoord.length === 0) {
                    return (ltreeFunc(ltree))
                } else {
                    const [i, ...tail] = lcoord
                    return produce(
                        ltree, draft => {
                            draft.children[i] = fix(fix)(tail, ltreeFunc, ltree.children[i])
                        })
                }
            })
            const newtree = f(f)(coord, treeFunc, tree)
            return (editAt(rest, treeFunc, newtree))
        }
    }

export const getAt: (c: TreeIndex, t: Treeable) => Treeable
    = (coord, tree) => {
        if (coord.length === 0) {
            return tree
        } else {
            const [head, ...tail] = coord
            return getAt(tail, tree.children[head])
        }
    }

// fold : Tree -> Tree
export const foldToggle: (t: Treeable<BasicTag, unknown>) => Treeable<BasicTag, unknown>
    = (tree) => {return produce(tree, draft => {draft.folded = !draft.folded})}
