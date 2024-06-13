import React from "react"
import { create } from "zustand"
import { devtools } from "zustand/middleware"

interface State {
    notifications: Config[]
    push: (config: Config) => void
    close: (index: number) => void
}

export const useStore = create<State>()(devtools(
    (set) => ({
        notifications: [],
        push: (config) => set(
            (state) => ({ notifications: [...state.notifications, config] })
        ),
        close: (index) => set(
            (state) => ({ notifications: state.notifications.filter((_, i) => i !== index) })
        ),
    })
))

export enum Kind {
    Success,
    Error,
    Info,
    Warn,
}

export type Config = {
    kind: Kind
    message: string
    detail?: string
}

export const Notifications: React.FC = () => {
    const notifications = useStore((state) => state.notifications)
    const close = useStore((state) => state.close)

    return (
        <div className="notifications">
            {notifications.map((config, i) => (
                <Notification key={i} config={config} close={() => close(i)} />
            ))}
        </div>
    )
}

const Notification: React.FC<{ config: Config, close: () => void }> = ({ config, close }) => {
    const cls = () => {
        switch (config.kind) {
            case Kind.Success: return "bg-green"
            case Kind.Error: return "bg-red"
            case Kind.Info: return "bg-blue"
            case Kind.Warn: return "bg-orange"
        }
    }

    const icon = () => {
        switch (config.kind) {
            case Kind.Success: return "check-circle-o"
            case Kind.Error: return "times-circle-o"
            case Kind.Info: return "exclamation-circle"
            case Kind.Warn: return "exclamation-triangle"
        }
    }

    const title = () => {
        switch (config.kind) {
            case Kind.Success: return "Success"
            case Kind.Error: return "Error"
            case Kind.Info: return "Info"
            case Kind.Warn: return "Warning"
        }
    }

    return (
        <div className={`p2 mb2 notification ${cls()}`}>
            <div className="clearfix">
                <div className="left bold font-larger">
                    <i className={`fa fa-${icon()} mr2`} />
                    {title()}
                </div>
                <div className="right align-top right-align notification__close" onClick={() => close()}>
                    <i className="fa fa-times" />
                </div>
            </div>
            <div className="mt2">
                {config.message}
            </div>
            {
                config.detail &&
                <div className="font-smaller mt2">
                    {config.detail}
                </div>
            }
        </div>
    )
}
