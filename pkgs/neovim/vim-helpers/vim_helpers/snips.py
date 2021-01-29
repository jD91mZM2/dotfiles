def complete(search, options):
    """
    Based on some a query and a set of options, this function provides
    completion for all options starting with the search query.

    Example:

    ``` python
    complete(t[1], [ "access", "allow", "always" ])
    ```

    causes

    ```
    al_(low|ways)
    ```

    where _ is the mouse cursor
    """

    options.sort()

    matches = filter(lambda opt: opt.startswith(search), options)
    substrings = list(map(lambda opt: opt[len(search) :], matches))

    if len(substrings) == 1:
        return substrings[0]
    elif not matches:
        return ""
    else:
        return "(" + "|".join(substrings) + ")"
