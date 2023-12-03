// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

using System.Collections.Generic;
using Nuke.Common;
using Nuke.Common.CI.GitHubActions;
using Nuke.Common.Execution;
using Nuke.Common.IO;
using Nuke.Common.ProjectModel;
using Nuke.Common.Tools.DotNet;
using Nuke.Common.Utilities.Collections;
using Nuke.Components;
using Vipentti.Nuke.Components;
using static Nuke.Common.Tools.DotNet.DotNetTasks;
using static Vipentti.Nuke.Components.StandardNames;

[ExtendedGitHubActions(
    "pull-request",
    GitHubActionsImage.WindowsLatest,
    GitHubActionsImage.UbuntuLatest,
    GitHubActionsImage.MacOsLatest,
    OnPullRequestBranches = new[] { MainBranch, DevelopBranch },
    PublishArtifacts = false,
    FetchDepth = 0 // fetch full history
    , SetupDotnetVersions = new[]
    {
        "8.x",
    }
    , InvokedTargets = new[]
    {
        nameof(ITest.Test),
        nameof(IUseLinters.InstallLinters),
        nameof(IUseLinters.Lint),
    })]
[DisableDefaultOutputForHost<Terminal>(DefaultOutput.Logo)]
class Build : NukeBuild, IUseDotNetFormat, IUseCsharpier, IUseFantomas, IUseLinters, IHazSolution, ITest, ICompile
{
    public T From<T>()
        where T : INukeBuild => (T)(object)this;

    public static int Main() => Execute<Build>(x => x.From<ICompile>().Compile);

    public Solution CurrentSolution => From<IHazSolution>().Solution;

    public IEnumerable<Project> TestProjects => CurrentSolution.GetAllProjects("*Tests*");

    bool IUseCsharpier.UseGlobalTool => false;
    bool IUseFantomas.UseGlobalTool => false;

    IEnumerable<AbsolutePath> IUseFantomas.DirectoriesToFormat => new[]
    {
        RootDirectory / "src",
        RootDirectory / "tests",
    };

    IEnumerable<IProvideLinter> IUseLinters.Linters => new IProvideLinter[]
    {
        // From<IUseDotNetFormat>().Linter,
        From<IUseCsharpier>().Linter,
        From<IUseFantomas>().Linter,
    };

    // csharpier-ignore
    public Target RestoreTools => _ => _
        .Before<IRestore>(it => it.Restore)
        .DependentFor<IUseLinters>(it => it.InstallLinters)
        .Executes(() => DotNetToolRestore())
        ;
}

public interface IUseFantomas : INukeBuild
{
    bool UseGlobalTool { get; }

    IEnumerable<AbsolutePath> DirectoriesToFormat => new[] { RootDirectory };

    // csharpier-ignore
    Target CheckFantomas => _ => _
        .Executes(() => RunFantomas(check: true));

    // csharpier-ignore
    Target InstallFantomas => _ => _
        .OnlyWhenDynamic(() => UseGlobalTool)
        .Executes(ExecuteInstallGlobalFantomas);

    // csharpier-ignore
    Target FormatFantomas => _ => _
        .TryBefore<IUseDotNetFormat>(x => x.Format)
        .TryDependentFor<IUseDotNetFormat>(x => x.Format)
        .Executes(() => RunFantomas(check: false));

    sealed IProvideLinter Linter =>
        new LinterDelegate(ExecuteInstallGlobalFantomas, () => RunFantomas(check: true));

    sealed void ExecuteInstallGlobalFantomas()
    {
        if (UseGlobalTool)
        {
            DotNetToolUpdate(_ => _.SetGlobal(true).SetPackageName("fantomas"));
        }
    }

    sealed void RunFantomas(bool check)
    {
        var toolname = UseGlobalTool ? "fantomas" : "tool run fantomas";

        DirectoriesToFormat.ForEach(RunFormat);

        void RunFormat(AbsolutePath path)
        {
            DotNet(
                arguments: $"{toolname} {path}" + (check ? " --check" : ""),
                logInvocation: true
            );
        }
    }
}
