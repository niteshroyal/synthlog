import 'office-ui-fabric-react/dist/css/fabric.min.css';
import TacleApp from './components/TacleApp';
import { AppContainer } from 'react-hot-loader';
import { initializeIcons } from 'office-ui-fabric-react/lib/Icons';
import * as React from 'react';
import * as ReactDOM from 'react-dom';

initializeIcons();

let isOfficeInitialized = false;

const title = 'SynthLog Add-in: TaCLe';

const render = (Component) => {
    ReactDOM.render(
        <AppContainer>
            <Component title={title} isOfficeInitialized={isOfficeInitialized} />
        </AppContainer>,
        document.getElementById('container')
    );
};

/* Render application after Office initializes */
Office.initialize = () => {
    isOfficeInitialized = true;
    render(TacleApp);
};

/* Initial render showing a progress bar */
// render(MercsApp);

if (module.hot) {
    module.hot.accept('./components/TacleApp', () => {
        const NextApp = require('./components/TacleApp').default;
        render(NextApp);
    });
}