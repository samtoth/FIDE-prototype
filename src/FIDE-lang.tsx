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

import {Treeable, Desugarable, Displayable, BasicTag, TreeIndex, DisplayProps, Ctx, getAt} from './Treeable'
import {ADT, matchI, matchP} from 'ts-adt'
import formDefinition, {getTree, TreeUUID, useFormDef} from 'features/formDefinition'
import {Foldable, Selectable} from 'CodeViewFeatures'
import Latex from '@matejmazur/react-katex'
import {constant, pipe} from 'fp-ts/lib/function'
import produce from 'immer'

type Ident = string
type Char = string
type FormLit = number | Char | string

type Forall = [boolean, Ident, FormCore, FormCore]
type Lam = [Ident, FormCore]
type App = [FormCore, FormCore]
type Var = [number]
type FTy = "type"
type Lit = FormLit


export type FormCore = ADT<{forall: {value: Forall}, lam: {value: Lam}, app: {value: App}, fvar: {value: Var}, ty: {value: FTy}, lit: {value: Lit}, def: {value: FormDefinition}}>;

type Context = TreeIndex[]

const contextGet = (context: Context, index: number) => {
    return context[(context.length - 1) - index]
}

interface Introduction<T extends BasicTag, C> extends FIDETree<T, C> {
    varName(): Ident,
    varType<T1 extends BasicTag, C1>(): FIDETree<T1, C1>
}

interface FIDETree<T extends BasicTag, C> extends Desugarable<T, C>, Displayable<T, C> {
    reduce<T1 extends BasicTag, C1>(context: Context): FIDETree<T1, C1>

    normalise<T1 extends BasicTag, C1>(context: Context): FIDETree<T1, C1>
}


type ForallTag = {name: string, context: Context, erased: boolean, bindName: Ident}

class ForallTree<T1 extends BasicTag, C1, T2 extends BasicTag, C2>
    extends Treeable<ForallTag, [FIDETree<T1, C1>, FIDETree<T2, C2>]>
    implements FIDETree<ForallTag, [FIDETree<T1, C1>, FIDETree<T2, C2>]> {
    constructor(term: Forall, context: Context, index: TreeIndex) {
        const [erased, ident, ty, rty] = term
        super(
            {
                name: "forall",
                erased: erased,
                bindName: ident,
                context: context
            },
            false,
            [termToTree(ty, context, [...index, 0]), termToTree(rty, [...context, index], [...index, 1])]
        )
    }
    reduce<T1 extends BasicTag, C1>(context: Context): FIDETree<T1, C1> {
        throw new Error('Method not implemented.')
    }
    normalise<T1 extends BasicTag, C1>(context: Context): FIDETree<T1, C1> {
        throw new Error('Method not implemented.')
    }

    varName() {
        return this.tag.bindName
    }

    display(coord, props) {
        //const innerparam = (s, e) => (s + this.bindName + " ^{" + latexTree(tree.children[0]) + "}" + e)
        //const param = tree.tag.erased ? innerparam("{{ ", " }}") : innerparam("( ", " )")
        //const rhs = tree.folded ? "(...)" : latexTree(tree.children[1])
        //return ("\\forall " + param + ".\\ " + rhs)

        return (<Selectable coord={coord} treeId={undefined} active={props.active}>
            <Foldable folded={this.folded}>
                <div className="flex space-x-2">
                    <div className="flex space-x-2">
                        <div className="katex katex-html base">
                            <span className="mord" >∀</span>
                            <span className="mopen">(</span>
                            <span className="mord text textit">{this.tag.bindName}</span>
                            <span className="mrel mx-1" >:</span>

                        </div>
                        {this.children[0].display([...coord, 0], props)}
                        <div className="katex katex-html base">
                            <span className="mclose">)</span>
                            <span className="mord">.</span>
                        </div>
                    </div>
                    <div>{this.children[1].display([...coord, 1], props)}</div>
                </div>
            </Foldable>
        </Selectable >)
    }

    desugar() {
        return forall(this.tag.erased, this.tag.bindName, this.children[0].desugar(), this.children[1].desugar());
    }

}

type LamTag = {
    name: string,
    bindName: Ident,
    context: Context,
}

class LamTree<T, C>
    extends Treeable<LamTag, [FIDETree<T, C>]>
    implements Introduction<LamTag, [FIDETree<T, C>]>{
    constructor(term: Lam, context: Context, index: TreeIndex) {
        const [bindName, rhs] = term
        super({name: "lam " + bindName, bindName: bindName, context: context}, false, [termToTree(rhs, [...context, index], [...index, 0])])
    }

    desugar() {
        return lam(this.tag.bindName, this.children[0].desugar())
    }

    varName(): string {
        return this.tag.bindName
    }

    display(coord: TreeIndex, props: DisplayProps) {
        //const rhs = tree.folded ? "(...)" : latexTree(tree.children[0])
        //return ("\\lambda " + tree.tag.bindName + ".\\ " + rhs)
        return (
            <Selectable coord={coord} active={props.active}>
                <div className="flex space-x-2">
                    <div className="katex base">
                        <span className="mord mathnormal">λ</span>
                        <span className="mord text textit">{this.tag.bindName}.</span>
                    </div>
                    <Foldable folded={this.folded}>
                        {this.children[0].display([...coord, 0], props)}
                    </Foldable>
                </div>
            </Selectable>)
    }
}

type AppTag = {
    name: string,
    context: Context,
}

class AppTree<T1 extends BasicTag, C1, T2 extends BasicTag, C2>
    extends Treeable<AppTag, [FIDETree<T1, C1>, FIDETree<T2, C2>]>
    implements FIDETree<AppTag, [FIDETree<T1, C1>, FIDETree<T2, C2>]>{
    constructor(term: App, context: Context, index: TreeIndex) {
        const [func, arg] = term
        super({name: "app", context: context}, false, [termToTree(func, context, [...index, 0]), termToTree(arg, context, [...index, 0])])
    }

    desugar() {
        return app(this.children[0].desugar(), this.children[1].desugar())
    }

    display(coord: TreeIndex, props: DisplayProps) {
        return (<Selectable coord={coord} active={props.active}>
            <div className="flex space-x-2">
                <span>(</span>
                <Foldable folded={this.folded}>
                    {this.children[0].display([...coord, 0], props)}
                    {this.children[1].display([...coord, 1], props)}
                </Foldable>
                <span>)</span>
            </div>
        </Selectable>
        )
    }
}

type VarTag = {
    name: string,
    varIdx: number,
    context: Context
}

class VarTree
    extends Treeable<VarTag, []>
    implements FIDETree<VarTag, []>
{
    constructor(term: Var, context: Context, i: TreeIndex) {
        const [index] = term
        super({name: `var: ${index}`, varIdx: index, context: context}, false, [])
    }

    display(coord: TreeIndex, props: DisplayProps) {
        let varCoord = contextGet(this.tag.context, this.tag.varIdx)
        if (varCoord === undefined) {
            varCoord = this.tag.context[0]
        }
        return (<Selectable coord={coord} active={props.active}>
            <span className="katex katex-html"><span className="mord text textit"><NameOf id={undefined} coord={varCoord} /><span className="text-sm">{this.tag.varIdx}</span></span></span>
        </Selectable >)
    }

    desugar() {
        return fvar(this.tag.varName)
    }
}

const NameOf = (props: {coord: TreeIndex, id: TreeUUID}) => {
    const trees = useFormDef()
    const tree = getTree(trees, props.id)


    return (<>{getAt(props.coord, tree.tree).varName()}</>)
}

type TypeTag = {
    name: string,
    context: Context
}

class TypeTree
    extends Treeable<TypeTag, []>
    implements FIDETree<TypeTag, []> {
    constructor(context: Context, _i: TreeIndex) {
        super({name: "type", context: context}, false, [])
    }

    desugar() {
        return ty
    }

    display(coord, props: DisplayProps) {
        return <Selectable coord={coord} active={props.active}><span className="katex katex-html base"><span className="mord">⋆</span></span></Selectable>
    }
}

type LitTag = {
    name: string,
    val: FormLit,
    context: Context
}

class LitTree
    extends Treeable<LitTag, []>
    implements FIDETree<LitTree, []>
{
    constructor(lit: FormLit, context: Context, i: TreeIndex) {
        super({name: "lit " + lit, val: lit, context: context}, false, [])
    }

    desugar() {
        return lit(this.tag.val)
    }

    display(coord: TreeIndex, props: DisplayProps) {
        return (<Selectable coord={coord} active={props.active}><span>{this.tag.val}</span></Selectable>)
    }
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


export const termToTree: (form: FormCore, context: Context, index: TreeIndex) => Treeable<any, any>
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



//  record definition where
//      field
//          bindName : String
//          type     : FormCore
//          term     : FormCore
//

type FormDefinition = [Ident, FormCore, FormCore]


export const makeDef = (bindName, type, term) => {
    return {
        _type: "def", value: [bindName, type, term]
    }
}

type DefTag = {
    name: string,
    defName: Ident,
}

class DefTree<T1 extends BasicTag, C1, T2 extends BasicTag, C2>
    extends Treeable<DefTag, [FIDETree<T1, C1>, FIDETree<T2, C2>]>
    implements FIDETree<DefTag, [FIDETree<T1, C1>, FIDETree<T2, C2>]> {
    constructor(term: FormDefinition, context: Context, i: TreeIndex) {
        const [name, ty, expr] = term
        super({name: "def: " + name, defName: name}, false, [termToTree(ty, context, [...i, 0]), termToTree(expr, context, [...i, 1])])
    }

    desugar() {
        return makeDef(this.tag.bindName, this.children[0].desugar(), this.children[1].desugar())
    }

    display(coord: TreeIndex, props: DisplayProps) {
        //return (`${tree.tag.defName}^{${latexTree(tree.children[0])}}\\ \\leftarrow\\ ${latexTree(tree.children[1])}`)
        return (
            <Selectable coord={coord} treeId={undefined} active={props.active}>
                {this.folded ? <div className="font-bold">{this.tag.defName}</div> : <></>}
                <Foldable folded={this.folded}>
                    <div className="flex flex-col text-lg">
                        <div className="flex space-x-2"><span className="font-bold">{this.tag.defName}</span> <span>:</span> {this.children[0].display([...coord, 0], props)}</div>
                        <div className="flex space-x-2"><span className="font-bold">{this.tag.defName}</span> <span>=</span> {this.children[1].display([...coord, 1], props)}</div>
                    </div>
                </Foldable>
            </Selectable >
        )
    }
}


const reduce = (term: FormCore, context: Ctx) => {
    return pipe(term,
        matchP({

        },
            (t) => t)
    )
}


const normalise = () => {

}

