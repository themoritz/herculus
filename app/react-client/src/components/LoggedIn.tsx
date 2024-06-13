import React from "react"
import { Link } from "react-router-dom"

export const LoggedIn: React.FC = () => {
    return (
        <div className="login">
            <Link to="/login">Log in</Link>
        </div>
    )
}
