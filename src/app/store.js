import {configureStore} from '@reduxjs/toolkit'
import counterReducer from '../features/counter'
import formDefReducer from '../features/formDefinition'
import logReducer from '../features/log'

export default configureStore({
    reducer: {
        counter: counterReducer,
        formDef: formDefReducer,
        log: logReducer
    }
})
