import React from 'react';
import { Kind, Notifications, useStore } from './Notifications';
import { BrowserRouter, Route, Routes } from 'react-router-dom';
import { ProjectOverview } from './ProjectOverview';
import { LogIn } from './auth/LogIn';
import { SignUp } from './auth/SignUp';
import { App } from './utils/Templates';

export const Root: React.FC = () => {
    const push = useStore((state) => state.push)

    return (
        <BrowserRouter>
            <div>
                <Notifications />
                <Routes>
                    <Route path="/" element={<App><ProjectOverview /></App>} />
                    <Route path="/projects" element={<App><ProjectOverview /></App>} />
                    <Route path="/login" element={<App><LogIn /></App>} />
                    <Route path="/signup" element={<App><SignUp /></App>} />
                </Routes>
                <button onClick={() => push({ kind: Kind.Warn, message: 'Hello !' })}>Push</button>
            </div>
        </BrowserRouter>
    )
}
