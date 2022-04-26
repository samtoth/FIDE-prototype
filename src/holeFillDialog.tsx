import {Dialog} from "@headlessui/react"

interface HFDProps {
    open: boolean,
    onClose: (e: any) => any
}

const HoleFillDialog = (props: HFDProps) => {
    return (<Dialog open={props.open} onClose={props.onClose} className="fixed z-10 inset-0 overflow-y-auto">
        <Dialog.Overlay className="fixed inset-0 bg-black opacity-30" />

        <div className="min-h-screen px-4 text-center">
            <span
                className="inline-block h-screen align-middle"
                aria-hidden="true"
            >
                &#8203;
            </span>
            <div className="inline-block w-full max-w-md p-6 my-8 overflow-hidden text-left align-middle transition-all transform bg-white shadow-xl rounded-2xl">

                <Dialog.Title className="font-bold py-2 border-b">
                    Fill Hole
                </Dialog.Title>

                <button>var 1</button>

                <button>var 2</button>
            </div>
        </div>
    </Dialog>)
}


export default HoleFillDialog
