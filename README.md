[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# scroll-text
> Scroll the text for content.

[![CI](https://github.com/jcs-elpa/scroll-text/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/scroll-text/actions/workflows/test.yml)

<p align="center">
  <img src="./etc/demo.gif"/>
</p>

## Usage

You can start scroll text animation in any writable buffer with any text at
anywhere inside buffer. For instance, The following code will display
`Hello World!~` in the beginning of the buffer.

```el
(scroll-text-start "Hello World!~" (point-min))
```

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
