// date FormCore where
//      forall : Bool -> Ident -> (type : FormCore) -> (rtype : FormCore) -> FormCore
//      lam : Ident -> (body : FormCore) -> FormCore
//      app : FormCore -> FormCore -> FormCore
//      var : Ident -> FormCore
//      type : FormCore
//      lit : FormLit -> FormCore
//
//
// FormCore  = (Bool -> Ident -> (type : FormCore) -> (rtype : FormCore) -> a) 
//          -> (Ident -> FormCore -> a)
//          -> (FormCore -> FormCore -> a) 
//          -> (Ident -> a)
//          -> a
//          -> (FormLit -> a)
//          -> a
//
// FormLit   = (Num -> a)
//          -> (Char -> a)
//          -> (String -> a)
//
//  record definition where
//      field
//          bindName : String
//          type     : FormCore
//          term     : FormCore
//



import {BasicTag, Desugarable, Displayable, Treeable, TreeIndex, WithContext} from './Treeable'
import {ADT, matchI} from 'ts-adt'
import produce from 'immer'
import {ForallTree} from "./ForallTree";
import {LamTree} from "./LamTree";
import {AppTree} from "./AppTree";
import {VarTree} from "./VarTree";
import {TypeTree} from "./TypeTree";
import {LitTree} from "./LitTree";
import {DefTree} from "./DefTree";

export type FormDefinition = [Ident, FormCore, FormCore]


export type Ident = string
export type Char = string
export type FormLit = number | Char | string

export type Forall = [boolean, Ident, FormCore, FormCore]
export type Lam = [Ident, FormCore]
export type App = [FormCore, FormCore]
export type Var = [number]
export type FTy = "type"
export type Lit = FormLit


export type FormCore = ADT<{forall: {value: Forall}, lam: {value: Lam}, app: {value: App}, fvar: {value: Var}, ty: {value: FTy}, lit: {value: Lit}, def: {value: FormDefinition}}>;

export type Context = TreeIndex[]

export const contextGet = (context: Context, index: number) => {
    return context[(context.length - 1) - index]
}

export interface Introduction<T extends BasicTag, C> extends FIDETree<T, C> {
    varName(): Ident,
    varType(): FIDETree<BasicTag, unknown>
}

export interface FIDETree<T extends BasicTag, C> extends Desugarable<T, C>, Displayable<T, C> {
    reduce(): FIDETree<BasicTag, unknown>

    normalise(): FIDETree<BasicTag, unknown>

    substitute<T1 extends BasicTag, C1>(idx: number, tree: Treeable<T1, C1>): FIDETree<T, C> | Treeable<T1, C1>

    contextProp(current: TreeIndex, target: TreeIndex, context: Context): Context
}

export const gatherContext = (t: FIDETree<BasicTag, unknown>, idx: TreeIndex): Context => {
    return t.contextProp([], idx, [])
}

export function reduceAndNormalise(tree: FIDETree<BasicTag, unknown>): FIDETree<BasicTag, unknown>{
    const wn = tree.reduce()
    return wn.normalise()
    console.log("postnormal")
}

const contextApp = (context: Context, t: TreeIndex,): Context => {
    return [...context, t]
}

export const forall: (b: boolean, i: Ident, t: FormCore, r: FormCore) => FormCore = (vis, name, type, rtype) => {
    return {
        _type: "forall", value: [vis, name, type, rtype]
    }
}

export const lam: (n: Ident, body: FormCore) => FormCore = (name, body) => {
    return {
        _type: "lam", value: [name, body]
    }
}

export const app: (f: FormCore, a: FormCore) => FormCore = (func, arg) => {
    return {
        _type: "app", value: [func, arg]
    }
}

export const fvar: (n: number) => FormCore = name => {
    return {
        _type: "fvar", value: [name]
    }
}

export const ty: FormCore = {_type: "ty", value: "type"}

export const lit = (formlit: FormLit): FormCore => ({
    _type: "lit", value: formlit
})

export const makeDef = (bindName: string, type: FormCore, term: FormCore): FormCore => {
    return {
        _type: "def", value: [bindName, type, term]
    }
}

export const termToTree: (form: FormCore, context: Context, index: TreeIndex) => FIDETree<BasicTag, unknown>
    = (term: FormCore, context: Context, index: TreeIndex) => {
        return matchI(term)({
            forall: ({value}) => new ForallTree(value, context, index),
            lam: ({value}) => LamTree.fromForm(value, context, index),
            app: ({value}) => AppTree.fromForm(value, context, index),
            fvar: ({value}) => new VarTree(value, context, index),
            ty: () => new TypeTree(context, index),
            lit: ({value}) => new LitTree(value, context, index),
            def: ({value}) => new DefTree(value, context, index)
        })
    }


export const printFormJs = (term: FormCore): string => {
    return matchI(term)({
        forall: ({value: [erased, name, type, rtype]}) => {
            const start = erased ? "%" : "@"
            const bind: string = "(" + name + " : " + printFormJs(type) + " )"
            return (start + bind + " " + printFormJs(rtype))
        },
        lam: ({value: [name, body]}) => {
            return ("#" + name + " " + printFormJs(body))
        },
        app: ({value: [func, arg]}) => {
            return ("(" + printFormJs(func) + " " + printFormJs(arg) + ")")
        },
        fvar: ({value: [fvarName]}) => {
            return `${fvarName}`
        },
        ty: (_v) => "*",
        lit: ({value: lit}) => {
            return `${lit}`
        },
        def: ({value: [name, type, expr, arg]}) => {
            return `${name} : ${printFormJs(type)} = ${printFormJs(expr)}`
        }
    })
}