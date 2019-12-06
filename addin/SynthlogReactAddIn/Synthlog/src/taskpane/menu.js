import 'office-ui-fabric-react/dist/css/fabric.min.css';
import MenuApp from './components/MenuApp';
import { AppContainer } from 'react-hot-loader';
import { initializeIcons } from 'office-ui-fabric-react/lib/Icons';
import * as React from 'react';
import * as ReactDOM from 'react-dom';

initializeIcons();

let isOfficeInitialized = false;

const title = 'SynthLog Add-in';

const render = (Component) => {
    ReactDOM.render(
        <AppContainer>
            <Component title={title} isOfficeInitialized={isOfficeInitialized} />
        </AppContainer>,
        document.getElementById('containerMenu')
    );
};

/* Render application after Office initializes */
Office.initialize = () => {
    isOfficeInitialized = true;
    render(MenuApp);
};

/* Initial render showing a progress bar */
render(MenuApp);

if (module.hot) {
    module.hot.accept('./components/MenuApp', () => {
        const NextApp = require('./components/MenuApp').default;
        render(NextApp);
    });
}