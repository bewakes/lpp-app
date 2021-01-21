import React from 'react';
import ReactDOM from 'react-dom';


import Solver from './Solver';

import './style.scss';

const App = () => {
    return (
        <div>
            <div id="nav">
                <h2><u>LPP Solver</u></h2>
            </div>
            <Solver />
        </div>
    );
};


ReactDOM.render(
    <App />,
    document.getElementById('app')
);
