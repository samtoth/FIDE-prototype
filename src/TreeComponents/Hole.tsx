import {Selectable} from "CodeViewFeatures";
import {info} from "features/log";
import {app, fvar, lit} from "TreeComponents/FIDE-lang";
import {useEffect, useState} from "react";
import {useDispatch} from "react-redux";
import {BasicTag, Desugarable, Displayable, DisplayProps, Treeable, TreeIndex} from "TreeComponents/Treeable";

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
            <Component {...props} />
        </Selectable>)
    }

}

const Component = (props: DisplayProps) => {
    const dispatch = useDispatch()
    const [dialogOpen, setDialogOpen] = useState(false)


    useEffect(() => {
        const containerRef = document.querySelector(`#tree${props.treeId}`)
        console.log(containerRef, `tree${props.treeId}`)
        containerRef?.addEventListener("click", (e) => {
            console.log("clocked")
            setDialogOpen(false)
        }, false)
        return () => {
            //containerRef?.removeEventListener("click")
        }

    }, [])

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
                    {
                    }
                    <div className="block block px-4 py-2 text-md text-gray-700 hover:bg-gray-100 hover:text-gray-900 ">
                        var 0
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
