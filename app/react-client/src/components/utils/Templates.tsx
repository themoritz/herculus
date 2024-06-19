import React, { ReactNode } from "react"

type AppProps = {
    navigation?: ReactNode
    header?: ReactNode
    children: ReactNode
}

export const App: React.FC<AppProps> = ({ navigation, header, children }) => {
    return (
        <div className="container">
            <div className="menubar">
                <div className="clearfix">
                    <div className="left clearfix">
                        <img src="img/herculus.svg" className="herculus__logo left align-baseline m1" />
                        <div className="herculus__text bold left">Herculus</div>
                    </div>
                    <div className="right m2">
                        {navigation}
                    </div>
                </div>
                {header}
            </div>
            <div className="content-body">
                {children}
            </div>
        </div>
    )
}

type RowProps = {
    label: string
    name: string
    password?: boolean
    value: string
    onInput: (value: string) => void
    onEnter: () => void
}

export const Row: React.FC<RowProps> = ({ label, name, password, value, onInput, onEnter }) => {
    return (
        <tr>
            <td className="py1 auth-form__cell">
                <label htmlFor={name}>{label}</label>
            </td>
            <td className="py1 auth-form__cell">
                <input
                    name={name}
                    className="input"
                    type={password ? 'password' : 'text'}
                    value={value}
                    onInput={(e) => onInput(e.currentTarget.value)}
                    onKeyDown={(e) => e.key === 'Enter' && onEnter()}
                />
            </td>
        </tr>
    )
}

export const Submit: React.FC<{ onClick: () => void }> = ({ onClick }) => {
    return (
        <tr>
            <td className="py1 right-align" colSpan={2}>
                <button className="button" onClick={onClick}>Submit</button>
            </td>
        </tr>
    )
}
