#!/usr/bin/env node
const inquirer = require("inquirer");
inquirer.prompt([
    {
        type: "input",
        name: "name",
        message: "Name:",
        default: "World"
    }
]).then((answers) => {
    console.log(`Hello, ${answers.name}!`);
});
