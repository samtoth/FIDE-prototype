import {createSlice} from "@reduxjs/toolkit";
import {useSelector} from "react-redux";

const minfo = 0
const mwarning = 1
const merror = 2

export const info = (message, extra) => {
    return send({type: minfo, message, timeout: extra.timeout})
}

export const warning = (message, extra) => {
    return send({type: mwarning, message, timeout: extra.timeout})
}

export const error = (message, extra) => {
    return send({type: merror, message, timeout: extra.timeout})
}

export const logSlice = createSlice(
    {
        name: "log",
        initialState: {
            messages: []
        },
        reducers: {
            send: (state, action) => {
                state.messages.push({type: action.payload.type, message: action.payload.message, extra: action.payload.extra, timestamp: Date.now(), timeout: action.payload.timeout})
            },
        }
    }
)

export const useLogs = () => useSelector(state => state.log)

export const {send} = logSlice.actions
export default logSlice.reducer
