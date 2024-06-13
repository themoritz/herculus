import React from 'react';
import { createRoot } from 'react-dom/client';
import { Root } from './components/Root';
import './css/main.css';

const container = document.getElementById('root');
const root = createRoot(container!);
root.render(<Root />);
