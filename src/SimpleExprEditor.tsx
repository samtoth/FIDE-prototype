import CodeTree from './CodeTree'
import {cursor_child, cursor_parent, cursor_sibling_dec, cursor_sibling_inc, cursor_unset, editTree,
    replaceWithNormal, TreeUUID, useFormDef} from './features/formDefinition'
import {useDispatch} from 'react-redux'
import {useState} from 'react'
import {info} from 'features/log'
import {BasicTag, Displayable, foldToggle, Treeable} from 'TreeComponents/Treeable'
import HoleTree, {replaceWithHole} from 'TreeComponents/Hole'
import HoleFillDialog from 'holeFillDialog'
import {AppTree} from "./TreeComponents/AppTree";
import {VarTree} from "./TreeComponents/VarTree";
import {reduceAndNormalise} from "./TreeComponents/FIDE-lang";

interface ExprEditorProps {
    treeId: TreeUUID | undefined,
    className: string | undefined
}

const SimpleExprEditor: (p: ExprEditorProps) => JSX.Element = ({treeId, className}) => {
    const trees = useFormDef()
    const tree = treeId === undefined ? trees.mainTree : trees.otherTrees.get(treeId)
    const cname = (className || "") + " flex flex-row"
    const dispatch = useDispatch()

    const [active, setActive] = useState(false)
    const [keys, setKeys] = useState([])



    const keyHandler = (e) => {
        //console.log(e)
        const code = (() => {
            let code = ""
            if (e.ctrlKey) {
                code += "C-"
            }
            if (e.altKey) {
                code += "A-"
            }

            return (code + e.code)
        })()
        if (code === "Escape") {
            setKeys([])
        }else if(code === "C-ControlLeft" || code === "C-ControlRight" || code === "A-AltLeft") {

        } else {
            const command = KeyMapping(treeId)([...keys, code])
            if (command) {
                e.preventDefault()
                dispatch(command)
                setKeys([])
            } else {
                dispatch(info(`keydown ${e.code}`, {timeout: 3000}))
                setKeys(keys => [...keys, code])
            }
        }
    }

    if (tree === undefined) {
        return (<div className={className}>Error tree {treeId} not found</div>)
    } else {
        return (
            <div className={cname}
                tabIndex={0}
                onKeyDown={keyHandler}
                onFocus={() => setActive(true)}
                onBlur={(_) => setActive(false)}>

                <CodeTree
                    tree={tree.tree}
                    selected={tree.cursor}
                    className="p-5 mr-5 font-semibold text-stone-100 bg-stone-700 shadow-inner max-h-full overflow-y-scroll"
                    coord={[]}
                    treeID={undefined} />
                <div className="grow min-h-full cursor-default m-10" id={`tree${treeId}`} onClick={(_) => dispatch(cursor_unset({id: treeId}))}>
                    {(tree.tree as Displayable<BasicTag, any>).display([], {active: active, highlights: [], treeId: treeId, context: []})}
                </div>

            </div >
        )

    }
}

const KeyMapping = (treeId: TreeUUID): any => {
    return (s: string[]) => {
        const fillHoleWith = (nt) => {return (t) => {
            if (t.constructor.name === "HoleTree") {
                return nt
            }else {
                return t
            }
        }}

        const parseNumeric = (s: string) => {
            const match =  s?.match(/^(?:Digit)([0-9])$/)
            if (!match){
                return null
            } else {
                return match[1]
            }
        }

        const obj = {
            "ArrowLeft": cursor_sibling_dec({id: treeId}),
            "ArrowRight": cursor_sibling_inc({id: treeId}),
            "ArrowUp": cursor_parent({id: treeId}),
            "ArrowDown": cursor_child({id: treeId}),
            "KeyF": editTree({func: foldToggle, id: treeId}),
            "Delete": editTree({func: replaceWithHole, id: treeId}),
            "Space": info("fill hole", {timeout: 3000}),
            "C-KeyA": editTree({func: fillHoleWith(new AppTree(new HoleTree(), new HoleTree()))}),
            "A-KeyN": replaceWithNormal({})
        }
        const simple = obj[s[0]]
        if (simple){
            return simple
        }


        if (s[0] === "KeyV"){
            const num = parseNumeric(s[1])

            if (num) {
                return editTree({func: fillHoleWith(new VarTree([Number(num)]))})
            }else {
                console.log("not yet")
            }

        }
    }
}

export default SimpleExprEditor
