import {Selectable} from "CodeViewFeatures";
import {info} from "features/log";
import {app, Context, fvar, lit} from "TreeComponents/FIDE-lang";
import React, {useEffect, useState} from "react";
import {useDispatch} from "react-redux";
import {
    BasicTag,
    Desugarable,
    Displayable,
    DisplayProps, getAt,
    Treeable,
    TreeIndex,
    WithContext
} from "TreeComponents/Treeable";
import {getTree, useFormDef, editTree, editTreeAt} from "../features/formDefinition";
import {VarTree} from "./VarTree";
import {AppTree} from "./AppTree";
import {LamTree} from "./LamTree";
import LetTree from "./LetTree";

type HoleUUID = number

type HoleTag = {
    name: string,
    holeId: HoleUUID
}

class HoleTree
    extends Treeable<HoleTag, []>
    implements Displayable<HoleTag, []>
{
    constructor() {
        super({name: "???", holeId: 0}, false, [])
    }

    display(coord: TreeIndex, props: DisplayProps) {
        return (<Selectable coord={coord} active={props.active} treeId={undefined} >
            <Component {...props} coord={coord}/>
        </Selectable>)
    }

}

const Component = (props: DisplayProps & {coord: TreeIndex}) => {
    const dispatch = useDispatch()
    const [dialogOpen, setDialogOpen] = useState(false)

    const trees = useFormDef()

    // useEffect(() => {
    //     const containerRef = document.querySelector(`#tree${props.treeId}`)
    //     console.log(containerRef, `tree${props.treeId}`)
    //     containerRef?.addEventListener("click", (e) => {
    //         console.log("clocked")
    //         setDialogOpen(false)
    //     }, false)
    //     return () => {
    //         //containerRef?.removeEventListener("click")
    //     }
    //
    // }, [])

    return (<div className="w-5 h-5 overflow-visible">
        <div className="w-5 h-5 bg-stone-800 cursor-pointer"
            onClick={(e) => {
                e.stopPropagation()
                e.nativeEvent.stopPropagation()
                setDialogOpen(d => !d)
                console.log(dialogOpen)
                dispatch(holeFillDialog())
            }}>
        </div >
        {dialogOpen && (
            <div className="text-sm origin-top-right relative top-7 mt-2 w-56 divide-y rounded-md shadow-lg bg-white ring-1 ring-black ring-opacity-5">
                <div className="p-1 font-bold" role="none">
                    Quick fill
                </div>

                <div className="py-1" role="none">
                    <div className="pl-1">
                        local vars
                    </div>
                    {
                        props.context.map((vcoord, idx) => {
                            const tree = getTree(trees, props.treeId)?.tree
                            const targetItem = getAt(vcoord, tree)
                            if (targetItem) {
                                return (<div
                                    className="block block px-4 py-2 text-md text-gray-700 hover:bg-gray-100 hover:text-gray-900 katex katex-html"
                                    onClick={(e) => {
                                        setDialogOpen(false)
                                        dispatch(editTreeAt(
                                            {
                                                id: props.treeId,
                                                func: (_: any) => {
                                                    return (new VarTree([props.context.length - idx - 1]))
                                                },
                                                at: props.coord,
                                            }
                                        ))
                                    }}>
                                    <span
                                        className="text-xs font-bold text-black-800 mr-1">{props.context.length - idx - 1}</span>
                                    <span className="mord text textit">{targetItem.varName()}</span>
                                </div>)
                            }
                        })
                    }
                </div>
                <div className="py-1" role="none">
                    <div className="pl-1">
                        others
                    </div>
                    <div
                        className="block block px-4 py-2 text-md text-gray-700 hover:bg-gray-100 hover:text-gray-900 katex katex-html"
                        onClick={(e) => {
                            setDialogOpen(false)
                            dispatch(editTreeAt(
                                {
                                    id: props.treeId,
                                    func: (_: any) => {
                                        return (new AppTree(new HoleTree(), new HoleTree()))
                                    },
                                    at: props.coord,
                                }
                            ))
                        }}>
                        <span className="mord text textit">Apply</span>
                    </div>
                    <div
                        className="block block px-4 py-2 text-md text-gray-700 hover:bg-gray-100 hover:text-gray-900 katex katex-html"
                        onClick={(e) => {
                            setDialogOpen(false)
                            dispatch(editTreeAt(
                                {
                                    id: props.treeId,
                                    func: (_: any) => {
                                        return (new LamTree("test", new HoleTree()))
                                    },
                                    at: props.coord,
                                }
                            ))
                        }}>
                        <span className="mord text textit">Abstract</span>
                    </div>
                    <div
                        className="block block px-4 py-2 text-md text-gray-700 hover:bg-gray-100 hover:text-gray-900 katex katex-html"
                        onClick={(e) => {
                            setDialogOpen(false)
                            dispatch(editTreeAt(
                                {
                                    id: props.treeId,
                                    func: (_: any) => {
                                        return (new LetTree([new HoleTree()], new HoleTree()));
                                    },
                                    at: props.coord,
                                }
                            ))
                        }}>
                        <span className="mord text textit">Let ... in ...</span>
                    </div>
                </div>
            </div>
        )}
    </div >)
}

const holeFillDialog = () => {
    return info("fill hole", {timeout: 3000})
}

export const replaceWithHole =
    <T extends BasicTag, C>(_tree: Treeable<T, C>): HoleTree => {
        return new HoleTree()
    }

export default HoleTree
