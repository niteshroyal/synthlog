export default class ServerAPI {
    constructor(api_url = null) {
        if (api_url) {
            this.api_url = api_url;
        } else {
            this.api_url = "https://localhost:3001/api"
        }
    }

    setupSqlite() {
        return fetch(`${this.api_url}/init_sqlite_db`)
            .then(response => response.json())
    }

    getInitialState(filename) {
        const body = {
            filename: filename
        };

        return fetch(`${this.api_url}/initial_state`, {
            method: 'POST',
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(body)
        }).then((response) => response.json())
    }

    getState(state_id) {
        return this.postAndParse("get_state", {
            state_id: state_id
        });
    }

    getTaskSuggestions(filename, context) {
        return this.postAndParse("get_tasks", {
            filename: filename,
            context: context,
        });
    }

    executeTask(filename, task_id, context) {
        return this.postAndParse("execute_task", {
            filename: filename,
            task_id: task_id,
            context: context,
        });
    }

    postAndParse(api_name, body) {
        return fetch(`${this.api_url}/${api_name}`, {
            method: 'POST',
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(body)
        }).then((response) => {
            return response.json();
        });

    }

    log(message_type, message) {
        return fetch(`${this.api_url}/log?type=${message_type}&message=${message}`)
    }
}
