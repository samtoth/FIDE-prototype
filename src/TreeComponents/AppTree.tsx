import {BasicTag, DisplayProps, Treeable, TreeIndex} from "./Treeable";
import {Foldable, Selectable} from "../CodeViewFeatures";
import {app, App, Context, FIDETree, termToTree} from "./FIDE-lang";

type AppTag = {
    name: string,
    context: Context,
}

export class AppTree<T1 extends BasicTag, C1, T2 extends BasicTag, C2>
    extends Treeable<AppTag, [FIDETree<T1, C1>, FIDETree<T2, C2>]>
    implements FIDETree<AppTag, [FIDETree<T1, C1>, FIDETree<T2, C2>]> {
    constructor(term: App, context: Context, index: TreeIndex) {
        const [func, arg] = term
        super({
            name: "app",
            context: context
        }, false, [termToTree(func, context, [...index, 0]), termToTree(arg, context, [...index, 0])])
    }

    desugar() {
        return app(this.children[0].desugar(), this.children[1].desugar())
    }

    display(coord: TreeIndex, props: DisplayProps) {
        return (<Selectable coord={coord} active={props.active}>
                <div className="flex space-x-2">
                    <span>(</span>
                    <Foldable folded={this.folded}>
                        <>
                            {this.children[0].display([...coord, 0], props)}
                            {this.children[1].display([...coord, 1], props)}
                        </>
                    </Foldable>
                    <span>)</span>
                </div>
            </Selectable>
        )
    }
}