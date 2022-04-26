import TeX from '@matejmazur/react-katex';
import {useDispatch} from 'react-redux';
import {useFormDef} from './features/formDefinition';

const LatexView = (props) => {
    const def = useFormDef()

    return <div className="cursor-default">{def.mainTree.tree.display([])}</div>
}

export default LatexView
