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



import {BasicTag, Desugarable, Displayable, TreeIndex} from './Treeable'
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
    varType<T1 extends BasicTag, C1>(): FIDETree<T1, C1>
}

export interface FIDETree<T extends BasicTag, C> extends Desugarable<T, C>, Displayable<T, C> {
    reduce<T1 extends BasicTag, C1>(context: Context): FIDETree<T1, C1>

    normalise<T1 extends BasicTag, C1>(context: Context): FIDETree<T1, C1>
}

const contextApp = (context: Context, t: TreeIndex,): Context => {
    return produce(context, draft => draft.push(t))
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

export const ty: FormCore = {_type: "ty"}

export const lit: (l: FormLit) => FormCore = formlit => {
    return {
        _type: "lit", value: formlit
    }
}

export const makeDef = (bindName, type, term) => {
    return {
        _type: "def", value: [bindName, type, term]
    }
}

export const termToTree: (form: FormCore, context: Context, index: TreeIndex) => FIDETree<any, any>
    = (term: FormCore, context: Context, index: TreeIndex) => {
        return matchI(term)({
            forall: ({value}) => new ForallTree(value, context, index),
            lam: ({value}) => new LamTree(value, context, index),
            app: ({value}) => new AppTree(value, context, index),
            fvar: ({value}) => new VarTree(value, context, index),
            ty: () => new TypeTree(context, index),
            lit: ({value}) => new LitTree(value, context, index),
            def: ({value}) => new DefTree(value, context, index)
        })
    }


const litToTree = (lit) => {
    return lit(
        (num) => {return mkTree({name: "lit: " + num, latex: _ => num, value: num}, false, [])},
        (ch) => {return mkTree({name: "char: " + ch, latex: _ => ch, value: ch}, false, [])},
        (string) => {return mkTree({name: "string: " + string, latex: _ => string, value: string}, false, [])}
    )
}


export const printFormJs = (term) => {
    return matchI(term)({
        forall: ({value: [erased, name, type, rtype]}) => {
            const start = erased ? "%" : "@"
            const bind = "(" + name + " : " + printFormJs(type) + " )"
            return (start + bind + " " + printFormJs(rtype))
        },
        lam: ({value: [name, body]}) => {
            return ("#" + name + " " + printFormJs(body))
        },
        app: ({value: [func, arg]}) => {
            return ("(" + printFormJs(func) + " " + printFormJs(arg) + ")")
        },
        fvar: ({value: [fvarName]}) => {
            return fvarName
        },
        ty: (_v) => "*",
        lit: ({value: lit}) => {
            return "lit"
        },
        def: ({value: [name, type, expr, arg]}) => {
            return `${name} : ${printFormJs(type)} = ${printFormJs(expr)}`
        }
    })
}