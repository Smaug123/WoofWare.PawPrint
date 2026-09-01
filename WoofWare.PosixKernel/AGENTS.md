When writing a docstring, please don't use .NET's standard XML docstring format, but just do a flat `/// some text here` with the text you want to include.
I want to write WoofWare.PosixKernel's docstrings myself by hand, so that I understand what is shipping, and I use "is this formatted as XML" as a proxy for "have I written this personally".

When editing an existing XML doc in PosixKernel, match the XML style; in particular, don't rehearse implementation details or history, do use the "example" tag where appropriate, and think about what an external caller would need to know when using the documented object.
For example, if you are editing an existing XML docstring:

* not "Whether a resolution follows symlinks" but "Whether a syscall follows symlinks during resolution" (how else is the user supposed to know what does the resolution?)
* not "A message composed to accompany this type should not claim to report a measurement on a real kernel", because that's purely internal guidance
* not a list of evidence derived from a real kernel that the modelled behaviour is as we stated, because that belongs in comments, not user-facing docstrings
* not "Whether the directory at `inode` still holds an entry, which is what `rmdir(2)` answers ENOTEMPTY for", but "Whether the directory at <c>inode</c> still holds an entry." and a "remarks" section of "This is so that we can determine whether <c>rmdir(2)</c> returns <c>ENOTEMPTY</c>." (the original wording read like an unrelated interesting fact rather than motivation)
